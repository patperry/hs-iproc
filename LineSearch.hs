-----------------------------------------------------------------------------
-- |
-- Module     : LineSearch
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Perform a one-dimensional line search to find a step length satisfying
-- the strong Wolfe conditions (sufficient decrease and curvature condition).
-- Useful as part of a function minimization algorithm.  
--
-- This module implements the algorithm described by Mor&#233; and Thuente
-- (1994), which is guaranteed to converge after a finite number of
-- iterations. The implementation allows the user to lazily return extra
-- state with each function evaluation, potentially storing gradient or
-- Hessian information. The 'search' function returns the state at the
-- optimal step value.
--
module LineSearch (
    -- * Search parameters
    Control(..),
    defaultControl,

    -- * Searching
    search,
    searchM,
    Result(..),
    Warning(..),
    
    -- * Continuation-based interface
    SearchCont(..),
    searchCont,
    
    -- * References
    -- $references
    ) where

import Prelude hiding ( init )
import Data.Maybe( fromJust, isJust )
import Debug.Trace( trace )


-- | Parameters for the line search.
data Control =
    Control { 
          valueTol :: !Double       -- ^ minimum relative decrease in function
                                    --   value (default @1e-4@)
        , derivTol :: !Double       -- ^ minimum relative decrease in
                                    --   derivative magnitude (default @0.9@)
        , stepTol :: !Double        -- ^ minimum relative precision in step
                                    --   (default @1e-4@)
        , stepMin :: !Double        -- ^ minimum acceptable step
                                    --   (default @0.0@)
        , stepMax :: !Double        -- ^ maximum acceptable step
                                    --   (default @Infinity@)
        , extrapLower :: !Double    -- ^ lower step multiplier when
                                    --   extrapolating (default @1.1@)
        , extrapUpper :: !Double    -- ^ upper step multiplier when
                                    --   extrapolating (default @4.0@)
        , safeguardReset :: !Double -- ^ relative reset when trial
                                    --   step is outside interval or too
                                    --   close to endpoint (default @0.66@)
        , bisectionWidth :: !Double -- ^ minimum relative decrease in interval
                                    --   before performing bisection instead
                                    --   (default @0.66@)
        , iterMax :: !(Maybe Int)   -- ^ maximum number of iterations, or
                                    --   @Nothing@ if unbouned
                                    --   (default @Nothing@)
        , verbose :: !Bool          -- ^ indicates whether to print status
                                    --   to stderr after each iterate
                                    --   (default @False@)
        }
    deriving (Eq, Show)

-- | Default search parameters, as specified in the documentation for
-- 'Control'.
defaultControl :: Control
defaultControl =
    Control { valueTol = 1e-4
            , derivTol = 0.9
            , stepTol = 1e-4
            , stepMin = 0
            , stepMax = infty
            , extrapLower = 1.1
            , extrapUpper = 4.0
            , safeguardReset = 0.66
            , bisectionWidth = 0.66
            , iterMax = Nothing
            , verbose = False
            }
  where
    infty = 1.0 / (0.0 :: Double)

checkControl :: Control -> a -> a
checkControl c
    | not (valueTol c >= 0) =
        error $ "invalid valueTol: `" ++ show (valueTol c) ++ "'"
    | not (derivTol c >= 0) =
        error $ "invalid derivTol: `" ++ show (derivTol c) ++ "'"
    | not (stepTol c >= 0) =
        error $ "invalid stepTol: `" ++ show (stepTol c) ++ "'"
    | not (stepMin c >= 0) =
        error $ "invalid stepMin: `" ++ show (stepMin c) ++ "'"
    | not (stepMax c >= stepMin c) =
        error $ "invalid stepMax: `" ++ show (stepMax c) ++ "'"
              ++ " (stepMin is `" ++ show (stepMin c) ++ "')"
    | not (extrapLower c > 1) =
        error $ "invalid extrapLower: `" ++ show (extrapLower c) ++ "'"
    | not (extrapUpper c > extrapLower c) =
        error $ "invalid extrapUpper: `" ++ show (extrapUpper c) ++ "'"
              ++ " (extrapLower is `" ++ show (extrapLower c) ++ "')"
    | not (safeguardReset c > 0 && safeguardReset c < 1) =
        error $ "invalid safeguardReset: `" ++ show (safeguardReset c) ++ "'"
    | not (bisectionWidth c >= 0 && bisectionWidth c < 1) =
        error $ "invalid bisectionWidth: `" ++ show (bisectionWidth c) ++ "'"
    | isJust (iterMax c) && not (fromJust (iterMax c) > 0) =
        error $ "invalid iterMax: `" ++ show (iterMax c) ++ "'"
    | otherwise = id



-- | The result of a line search.
data Result a =
    Result { resultIter :: !Int     -- ^ iterate number
           , resultStep :: !Double  -- ^ step value
           , resultValue :: !Double -- ^ function value
           , resultDeriv :: !Double -- ^ function derivative
           , resultState :: a       -- ^ extra state (lazy)
           }
    deriving (Eq, Show)

-- | A warning to go along with a failed search.
data Warning = RoundErr  -- ^ rounding errors prevent further progress
             | WithinTol -- ^ step is within 'stepTol' of a valid step
             | AtStepMin -- ^ step is at 'stepMin'
             | AtStepMax -- ^ step is at 'stepMax'
             | AtIterMax -- ^ algorithm has gone through 'iterMax' iterations
    deriving (Eq, Show)


-- | A search continuation, allowing fine-grained control over objective
-- function evaluation.  Both 'search' and 'searchM' are implemented using
-- a 'SearchCont'.
data SearchCont = Converged      -- ^ the search algorithm has converged
                | Stuck Warning  -- ^ the search algorithm is stuck
                | InProgress !Double ((Double,Double) -> SearchCont)
                                 -- ^ the search algorith is in progress;
                                 --   the first field is the next trial
                                 --   step value; the second field is
                                 --   a function which takes the value
                                 --   and derivative at the trial step
                                 --   and returns a new continutaiton

-- | Returns a search continuation initialized with the given values.
searchCont :: Control
           -- ^ search parameters
           -> (Double, Double)
           -- ^ value and derivative at zero
           -> Double
           -- ^ initial step size
           -> SearchCont
           -- ^ result
searchCont c fdf0 step0 = let
    ls = initState c fdf0 step0
    in InProgress step0 $ \(f,df) -> step (Eval step0 f df) ls


-- | Perform a line search to find a step that satisfies the strong Wolfe
-- conditions.  Upon success return 'Right' and the step; upon failure
-- return 'Left' with a warning and the best step obtained during the search.
search :: Control
       -- ^ search parameters
       -> (Double -> (Double, Double, a))
       -- ^ function which maps step to value, derivative, and extra state
       -> (Double, Double, a)
       -- ^ value, derivative, and extra state at zero
       -> Double
       -- ^ initial step size
       -> Either (Warning, Result a) (Result a)
       -- ^ search result
search c fdf (f0,df0,a0) step0 =
    runIdentity $ searchM c (return . fdf) (f0,df0,a0) step0

newtype Identity a = Identity { runIdentity :: a }
instance Monad Identity where
    return = Identity
    {-# INLINE return #-}
    m >>= f = f $ runIdentity m
    {-# INLINE (>>=) #-}


-- | Monadic version of 'search'.
searchM :: (Monad m)
        => Control
        -- ^ search parameters
        -> (Double -> m (Double, Double, a))
        -- ^ function mapping step to value, derivative, and extra state
        -> (Double, Double, a)
        -- ^ value, derivative, and extra state at zero
        -> Double
        -- ^ initial step size
        -> m (Either (Warning, Result a) (Result a))
        -- ^ search result
{-# INLINE searchM #-}
searchM c fdf (f0,df0,a0) step0 =
    convergeM fdf 0 (Eval 0 f0 df0, a0) $
        searchCont c (f0,df0) step0

{-# INLINE convergeM #-}
convergeM :: (Monad m)
          => (Double -> m (Double, Double, a))
          -> Int
          -> (Eval, a)
          -> SearchCont
          -> m (Either (Warning, Result a) (Result a))
convergeM fdf i (e,a) k =
    case k of
        Converged -> return $ Right result
        Stuck w   -> return $ Left (w, result)
        InProgress step' ks -> do
            (f',df',a') <- fdf step'
            let i' = i + 1
                k' = ks (f',df')
                e' = Eval step' f' df'
            i' `seq` e' `seq` convergeM fdf i' (e',a') k'
  where
    result = Result { resultIter = i
                    , resultStep = position e
                    , resultValue = value e
                    , resultDeriv = deriv e
                    , resultState = a 
                    }

data Eval = 
    Eval { position :: !Double
         , value :: !Double
         , deriv :: !Double
         }
    deriving Show

data State =
    State { control :: !Control
          , iter :: !Int
          , valueTest :: !Double
          , derivTest :: !Double
          , bracketed :: !Bool
          , stage1 :: !Bool
          , value0 :: !Double
          , deriv0 :: !Double
          , lowerEval :: !Eval
          , upperEval :: !Eval
          , testEval :: !Eval
          , stepLower :: !Double
          , stepUpper :: !Double
          , width :: !Double
          , width' :: !Double
          }

initState :: Control -> (Double, Double) ->  Double -> State
initState c (f0,d0) step0
    | isNaN f0 =
        error $ "initial value is NaN"
    | isNaN d0 =
        error $ "initial derivative is NaN"
    | not (d0 < 0) =
        error $ "initial derivative is not negative"
              ++ " (initial derivative is is `" ++ show d0 ++ "')"
    | not (stepMin c <= step0) =
          error $ "invalid step: `" ++ show step0 ++ "'"
                ++ " (stepMin is `" ++ show (stepMin c) ++ "')"
    | not (step0 <= stepMax c) =
          error $ "invalid step: `" ++ show step0 ++ "'"
                ++ " (stepMax is `" ++ show (stepMax c) ++ "')"
    | otherwise =
        let
            gtest = d0 * valueTol c
            ftest = f0 + step0 * gtest
            w = stepMax c - stepMin c
            lower = Eval { position = 0, value = f0, deriv = d0 }
            upper = lower
            test = lower
        in
            checkControl c $
                State { control = c
                      , iter = 0
                      , value0 = f0
                      , deriv0 = d0
                      , valueTest = ftest
                      , derivTest = gtest
                      , bracketed = False
                      , stage1 = True
                      , lowerEval = lower
                      , upperEval = upper
                      , testEval = test
                      , stepLower = 0
                      , stepUpper = step0 + extrapUpper c * step0
                      , width = w
                      , width' = 2 * w
                      }

step :: Eval -> State -> SearchCont
step test ls
    | isNaN (value test) =
        error $ "function value is NaN"
    | isNaN (deriv test) =
        error $ "function derivative is NaN"
    | (  value test <= ftest
      && abs (deriv test) <= ((derivTol $ control ls)
                              * (negate $ deriv0 ls))) =
        result $ Converged
    | brackt && t <= tmin || t >= tmax =
        stuck RoundErr
    | brackt && tmax - tmin <= (stepTol $ control ls) * tmax =
        stuck WithinTol
    | (  t == (stepMax $ control ls)
      && value test <= ftest
      && deriv test <= gtest ) =
        stuck AtStepMax
    | (  t == (stepMin $ control ls)
      && (  value test > ftest
         || deriv test >= gtest ) ) =
        stuck AtStepMin
    | isJust itmax && iter ls >= fromJust itmax =
        stuck AtIterMax
    | otherwise = let
        (t',ls') = unsafeStep test ls
        in result $ InProgress t' $ \(f',df') -> step (Eval t' f' df') ls'
  where
    stuck w = let
        t' = (position . lowerEval) ls
        in result $ InProgress t' $ \(f',df') -> Stuck w
        
    result | (not . verbose) (control ls) = id
           | otherwise =
                 trace ("iter: " ++ show (1 + iter ls)
                       ++ " step: " ++ show (position test)
                       ++ " value: " ++ show (value test)
                       ++ " deriv: " ++ show (deriv test)
                       )
    brackt = bracketed ls
    ftest = valueTest ls
    gtest = derivTest ls
    t = position test
    (tmin,tmax) = (stepLower ls, stepUpper ls)
    itmax = iterMax (control ls)


unsafeStep :: Eval -> State -> (Double, State)
unsafeStep test ls = let
    iter' = iter ls + 1
    (lower, upper) = (lowerEval ls, upperEval ls)
    ftest = valueTest ls
    gtest = derivTest ls
    
    -- if psi(t) <= 0 and f'(t) >= 0 for some step, then the
    -- algorithm enters the second stage
    stg1' = stage1 ls && (value test > ftest || deriv test < 0)
                 
    -- A modified function is used to predict the step during the
    -- first stage if a lower function value has been obtained but
    -- the decrease is not sufficient.
    modify e = if stg1' && value test <= value lower && value test > ftest
                   then e{ value = value e - position e * gtest
                         , deriv = deriv e - gtest }
                   else e
    (mlower, mupper, mtest) = (modify lower, modify upper, modify test)
    
    -- compute new step and update bounds
    (brackt', t0') = trialValue (safeguardReset $ control ls)
                                (stepLower ls, stepUpper ls) (bracketed ls)
                                (mlower, mupper) mtest
    (lower', upper') = updateIntervalWith (mlower,mupper,mtest)
                                          (lower,upper,test)
    
    -- perform a bisection step if necessary
    (w',w'',t1') =
        if brackt'
            then ( abs (position upper' - position lower')
                 , width ls
                 , if w' >= (bisectionWidth $ control ls) * width' ls
                       then (position lower'
                             + 0.5 * (position upper' - position lower'))
                       else t0'
                 )
            else ( width ls, width' ls, t0' )

    -- set the minimum and maximum steps allowed
    (tmin',tmax') =
        if brackt'
            then ( min (position lower') (position upper')
                 , max (position lower') (position upper')
                 )
            else ( t1' + (extrapLower $ control ls) * (t1' - position lower')
                 , t1' + (extrapUpper $ control ls) * (t1' - position lower')
                 )
    
    -- force the step to be within bounds
    t' = (max (stepMin $ control ls) . min (stepMax $ control ls)) t1'
    ftest' = value0 ls + t' * gtest
    
    in ( t'
       , ls{ iter = iter'
           , bracketed = brackt'
           , stage1 = stg1'
           , valueTest = ftest'
           , lowerEval = lower'
           , upperEval = upper'
           , stepLower = tmin'
           , stepUpper = tmax'
           , width = w'
           , width' = w''
           }
       )

-- | Modified updating algorithm (pp. 297-298)
updateIntervalWith :: (Eval, Eval, Eval)
                   -> (Eval, Eval, Eval)
                   -> (Eval, Eval)
updateIntervalWith ((Eval _l fl gl), (Eval _u _fu _gu), (Eval _t ft gt))
                   (lower, upper, test)
    -- Case a: higher function value
    | ft > fl =
        (lower, test)
    -- Case b: lower function value, derivatives different signs
    | otherwise && signum gt /= signum gl =
        (test, lower)
    -- Case c: lower function value, derivatives same sign
    | otherwise =
        (test, upper)


-- | Trial value selection (Sec. 4, pp. 298-300)
trialValue :: Double
           -> (Double, Double)
           -> Bool
           -> (Eval, Eval)
           -> Eval
           -> (Bool, Double)
trialValue sreset
           (tmin,tmax)
           brackt
           ((Eval l fl gl), (Eval u fu gu))
           (Eval t ft gt)
    | not (sreset > 0 && sreset < 1) =
        error $ "Invalid safeguard reset: `" ++ show sreset ++ "'"
    | brackt && not (min l u < t && t < max l u) =
        error $ "Trial value `" ++ show t ++ "' is out of the interval"
              ++ "`(" ++ show (min l u) ++ ", " ++ show (max l u) ++ "'"
    | brackt && not (gl * (t - l) < 0) =
        error $ "Function does not decrease from lower endpoint"
    | brackt && not (tmin <= tmax) =
        error $ "Trial max `" ++ show tmax ++ "'"
              ++ " is less than trial min `" ++ show tmin ++ "'"

    -- Case 1: higher function value
    | ft > fl =
        result True $
            if abs (c - l) < abs (q - l)
                then c
                else c + 0.5 * (q - c)

    -- Case 2: lower function value, derivative opposite sign
    | signum gt /= signum gl =
        result True $
            if abs (c - t) >= abs (s - t)
                then c
                else s

    -- Case 3: lower function value, derivatives same sign, lower derivative
    | abs gt <= abs gl = let
        c' = if (not . isNaN) c && signum (c - t) /= signum (l - t)
                 then c
                 else if t > l then tmax
                               else tmin
        in result brackt $
            case brackt of
               -- extrapolate to closest of cubic and secant steps
               True | abs (t - c') < abs (t - s) -> safeguard c'
               True | otherwise                  -> safeguard s

               -- extrapolate to farthest of cubic and secant steps
               False | abs (t - c') > abs (t - s) -> clip c'
               False | otherwise                  -> clip s
       
    -- Case 4: lower function value, derivatives same sign, higher derivative
    | otherwise =
        result brackt $
            case brackt of
                True              -> cubicMin (t,ft,gt) (u,fu,gu)
                False | t > l     -> tmax
                False | otherwise -> tmin
  where
    c = cubicMin (l,fl,gl) (t,ft,gt)
    q = quadrMin (l,fl,gl) (t,ft)
    s = secant (l,gl) (t,gt)

    clip = max tmin . min tmax

    safeguard | t > l     = min (t + sreset * (u - t))
              | otherwise = max (t + sreset * (u - t))

    result brackt' t' = (brackt',t')


quadrMin :: (Double,Double,Double)
         -> (Double,Double)
         -> Double
quadrMin (u,fu,du) (v,fv) = let
    a = v - u
	in u + (du / ((fu - fv) / a + du) / 2) * a
	
secant :: (Double,Double)
          -> (Double,Double)
          -> Double
secant (u,du) (v,dv) = let
	a = u - v
	in v + dv / (dv - du) * a

cubicMin :: (Double,Double,Double)
         -> (Double,Double,Double)
         -> Double
cubicMin (u,fu,du) (v,fv,dv) = let
	d = v - u
	theta = (fu - fv) * 3 / d + du + dv
	s = maximum [ abs theta, abs du, abs dv ]
	a = theta / s
	gamma0 = s * sqrt (a * a - (du / s) * (dv / s))
	gamma = if v < u then -gamma0 else gamma0
	p = gamma - du + theta
	q = gamma - du + gamma + dv
	r = p / q
	in u + r * d

-- $references
--
-- * Mor&#233;, J. J. and Thuente, D. J. (1994) Line search algorithms
--   with guaranteed sufficient decrease. /ACM Transactions on Mathematical Software/
--   20(3):286&#8211;307. <http://doi.acm.org/10.1145/192115.192132>
--
-- * Nocedal, J. and Wright, S. J. (2006) /Numerical Optimization/, 2nd ed.
--   Springer. <http://www.springer.com/mathematics/book/978-0-387-30303-1>
--
