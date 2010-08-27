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
-- (1994), which is guaranteed to converge after a finite number of steps.
-- The implementation allows the user to lazily return extra state with each
-- function evaluation, potentially storing gradient or Hessian information. 
-- The 'search' function returns the state at the optimal step value.
--
module LineSearch (
    -- * Search parameters
    Control(..),
    defaultControl,

    -- * Searching
    search,
    Result(..),
    Warning(..),
    
    -- * References
    -- $references
    ) where

import Prelude hiding ( init )
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
                                    --   (default @1e10@)
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
        , iterMax :: !Int           -- ^ maximum number of iterations
                                    --   (default @10^10@)
        , verbose :: !Bool          -- ^ print status to stderr after each
                                    --   iterate (default @False@)
        }
    deriving (Eq, Show)

-- | Default search parameters, as specified above.
defaultControl :: Control
defaultControl =
    Control { valueTol = 1e-4
            , derivTol = 0.9
            , stepTol = 1e-4
            , stepMin = 0
            , stepMax = 1e10
            , extrapLower = 1.1
            , extrapUpper = 4.0
            , safeguardReset = 0.66
            , bisectionWidth = 0.66
            , iterMax = 10^10
            , verbose = False
            }

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
    | not (iterMax c > 0) =
        error $ "invalid iterMax: `" ++ show (iterMax c) ++ "'"
    | otherwise = id



type Function a = Double -> (Double, Double, a)

data Eval a = 
    Eval { position :: !Double
         , value :: !Double
         , deriv :: !Double
         , state :: a
         }
    deriving Show

-- | The result of a line search.
data Result a =
    Result { resultIter :: !Int     -- ^ iterate number
           , resultStep :: !Double  -- ^ step value
           , resultValue :: !Double -- ^ function value
           , resultDeriv :: !Double -- ^ function derivative
           , resultState :: a       -- ^ extra state (lazy)
           }
    deriving (Eq, Show)

data LineSearch a =
    LineSearch { control :: !Control
               , function :: !(Function a)
               , valueTest :: !Double
               , derivTest :: !Double
               , bracketed :: !Bool
               , stage1 :: !Bool
               , value0 :: !Double
               , deriv0 :: !Double
               , lowerEval :: !(Eval a)
               , upperEval :: !(Eval a)
               , testEval :: !(Eval a)
               , stepLower :: !Double
               , stepUpper :: !Double
               , width :: !Double
               , width' :: !Double
               }

-- | Perform a line search to find a step that satisfies the
-- strong Wolfe conditions.
search :: Control
       -- ^ search parameters
       -> (Double -> (Double, Double, a))
       -- ^ function mapping step to value, derivative, and extra state
       -> (Double, Double, a)
       -- ^ value, derivative, and extra state at zero
       -> Double
       -- ^ initial step size
       -> Either (Warning, Result a) (Result a)
       -- ^ search result; upon failure return 'Left' with a warning
       --   and the best step obtained during the search
search c fdf (f0,df0,a0) step0 = converge 1 $ init c fdf (f0,df0,a0) step0

converge :: Int -> LineSearch a -> Either (Warning, Result a) (Result a)
converge iter ls
    | verbose (control ls) = let
        e = testEval ls
        in trace ("iter: " ++ show iter
                 ++ " step: " ++ show (position e)
                 ++ " value: " ++ show (value e)
                 ++ " deriv: " ++ show (deriv e)
                 ) result 
    | otherwise =
        result
    
  where
    result =
        if iter >= iterMax (control ls)
            then let
                e = lowerEval ls
                in if (value e > valueTest ls || deriv e >= derivTest ls)
                    then Left (AtIterMax, fromEval e)
                    else Right (fromEval e)
            else case step ls of
                Stuck w e      -> Left (w, fromEval e)
                Converged e    -> Right (fromEval e)
                InProgress ls' -> converge (iter + 1) ls'
    fromEval e =
        Result { resultIter = iter
               , resultStep = position e
               , resultValue = value e
               , resultDeriv = deriv e
               , resultState = state e
               }

init :: Control -> Function a -> (Double, Double, a) -> Double -> LineSearch a
init c fdf (f0,d0,a0) step0
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
            (f,d,a) = fdf step0
            gtest = d0 * valueTol c
            ftest = f0 + step0 * gtest
            w = stepMax c - stepMin c
            lower = Eval { position = 0, value = f0, deriv = d0, state = a0 }
            upper = lower
            test = Eval { position = step0, value = f, deriv = d, state = a }
        in
            checkControl c $
                LineSearch { control = c
                           , function = fdf
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

-- | A warning to go along with a failed search.
data Warning = RoundErr  -- ^ rounding errors prevent further progress
             | WithinTol -- ^ step is within 'stepTol' of a valid step
             | AtStepMin -- ^ step is at 'stepMin'
             | AtStepMax -- ^ step is at 'stepMax'
             | AtIterMax -- ^ we have gone through 'iterMax' iterations
             | NaNValueAt !Double 
                         -- ^ the function value or its derivative is NaN
                         --   at the specified trial step
    deriving (Eq, Show)

data Status a = Converged (Eval a)
              | Stuck Warning (Eval a)
              | InProgress (LineSearch a)

step :: LineSearch a -> Status a
step ls
    | isNaN (value test) || isNaN (deriv test) =
        Stuck (NaNValueAt (position test)) $ lowerEval ls
    | brackt && t <= tmin || t >= tmax =
        Stuck RoundErr $ lowerEval ls
    | brackt && tmax - tmin <= (stepTol $ control ls) * tmax =
        Stuck WithinTol $ lowerEval ls
    | (  t == (stepMax $ control ls)
      && value test <= ftest
      && deriv test <= gtest ) =
        Stuck AtStepMax test
    | (  t == (stepMin $ control ls)
      && (  value test > ftest
         || deriv test >= gtest ) ) =
        Stuck AtStepMin test
    | (  value test <= ftest
      && abs (deriv test) <= ((derivTol $ control ls)
                              * (negate $ deriv0 ls))) =
        Converged test
    | otherwise =
        InProgress $ unsafeStep ls
      
  where
    brackt = bracketed ls
    ftest = valueTest ls
    gtest = valueTest ls
    test = testEval ls
    t = position test
    (tmin,tmax) = (stepLower ls, stepUpper ls)


unsafeStep :: LineSearch a -> LineSearch a
unsafeStep ls = let
    (lower, upper, test) = (lowerEval ls, upperEval ls, testEval ls)
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

    -- obtain another function and derivative
    test' = let (f,g,a) = (function ls) t' in Eval t' f g a
    ftest' = value0 ls + t' * gtest
    
    in ls{ bracketed = brackt'
         , stage1 = stg1'
         , valueTest = ftest'
         , lowerEval = lower'
         , upperEval = upper'
         , testEval = test'
         , stepLower = tmin'
         , stepUpper = tmax'
         , width = w'
         , width' = w''
         }

-- | Modified updating algorithm (pp. 297-298)
updateIntervalWith :: (Eval a, Eval a, Eval a)
                   -> (Eval a, Eval a, Eval a)
                   -> (Eval a, Eval a)
updateIntervalWith ((Eval _l fl gl _), (Eval _u _fu _gu _), (Eval _t ft gt _))
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
           -> (Eval a, Eval a)
           -> Eval a
           -> (Bool, Double)
trialValue sreset
           (tmin,tmax)
           brackt
           ((Eval l fl gl _), (Eval u fu gu _))
           (Eval t ft gt _)
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
-- * Nocedal, J. and Wrigth, S. J. (2006) /Numerical Optimization/, 2nd ed.
--   Springer. <http://www.springer.com/mathematics/book/978-0-387-30303-1>
--
