-----------------------------------------------------------------------------
-- |
-- Module     : BFGS
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Minimize a function using the Broyden-Fletcher-Goldfarb-Shanno (BFGS)
-- method.
--
-- This module implements the BFGS algorithm, a quasi-Newton method
-- for function minimization.  Note that the method finds a local optimum,
-- which is not gauranteed to be the global optimum unless the objective
-- function is strictly convex.  The implementation allows the user to lazily
-- return extra state with each function evaluation, potentially storing
-- Hessian information. The 'minimize' function returns the state at the
-- optimal step value.
--
module BFGS (
    -- * Minimization parameters
    Control(..),
    defaultControl,

    -- * Minimizing
    minimize,
    minimizeM,
    Result(..),
    Warning(..),
    
    -- * Continuation-based interface
    MinimizeCont(..),
    minimizeCont,
    
    -- * References
    -- $references
    ) where

import Debug.Trace( trace )
import LineSearch( SearchCont )
import qualified LineSearch as LineSearch

import Numeric.LinearAlgebra


-- | Parameters for the minimization.
data Control =
    Control {
          linesearchControl :: !LineSearch.Control
                                    -- ^ control parameters for the line
                                    --   search
        , gradTol :: !Double        -- ^ maximum Euclidean norm of the 
                                    --   gradient before declaring
                                    --   convergence; the minimization
                                    --   algorithm stops when
                                    --   @||g|| <= gradTol * max(1, ||x||)@,
                                    --   where @g@ is the gradient and
                                    --   @x@ is the position
        , iterMax :: !Int           -- ^ maximum number of iterations
                                    --   (default @100@)
        , verbose :: !Bool          -- ^ indicates whether to print status
                                    --   to stderr after each iterate
                                    --   (default @False@)
        } deriving (Show)
  
-- | Default minimization parameters, as specified in the documentation for
-- 'Control'.
defaultControl :: Control
defaultControl =
    Control { linesearchControl = LineSearch.defaultControl
            , gradTol = 1e-5
            , iterMax = 100
            , verbose = False
            }
  
checkControl :: Control -> a -> a
checkControl c
    | not (gradTol c >= 0) =
        error $ "invalid gradTol: `" ++ show (gradTol c) ++ "'"
    | not (iterMax c > 0) =
        error $ "invalid iterMax: `" ++ show (iterMax c) ++ "'"
    | otherwise = id
    
data Eval =
    Eval { position :: !(Vector Double)
         , value :: !Double
         , gradient :: !(Vector Double)
         }
    
data State =
    State { control :: !Control
          , iter :: !Int
          , prevEval :: !Eval
          , curEval :: !Eval
          , invHessian :: !(Maybe (Herm Matrix Double))
          , searchDir :: !(Vector Double)
          , searchPos :: !(Vector Double)
          , linesearch :: !((Double, Double) -> SearchCont)
          }

-- | A warning to go along with a failed minimization.
data Warning =
      LineSearch LineSearch.Warning -- ^ line search failed
    | AtIterMax                     -- ^ algorithm has gone through 'iterMax'
                                    --   iterations
    deriving (Eq, Show)

-- | A minimization continuation, allowing fine-grained control over
-- objective function evaluation. Both 'minimize' and 'minimizeM' are
-- implemented using a 'MinimizeCont'.
data MinimizeCont =
      Converged      -- ^ minimization algorithm has converged
    | Stuck Warning  -- ^ minimization alrorithm is stuck
    | InProgress !(Vector Double)
                 !((Double, Vector Double) -> MinimizeCont)
                     -- ^ minimization algorithm is in progress: the
                     --   first field is the next trial position; the
                     --   second field is a function which takes the
                     --   value and gradient at the trial position a
                     --   returns a new continuation

-- | Returns a minimization continuation initialized with the given values.
minimizeCont :: Control
             -- ^ search parameters
             -> Vector Double
             -- ^ initial position
             -> (Double, Vector Double)
             -- ^ initial value and gradient
             -> MinimizeCont
             -- ^ result
minimizeCont c x0 fdf0 = let
    bfgs0 = initState c x0 fdf0
    bfgs = if (not . verbose) c
               then bfgs0
               else trace ("MINIMIZE"
                          ++ " iter: 0"
                          ++ " value: " ++ show (fst fdf0)
                          ++ " dec: "
                          ++ show (negate $ 
                                   snd fdf0 `dotVector` searchDir bfgs0 / 2)
                          ) bfgs0
    x = searchPos bfgs
    
    in InProgress x $ \(f,df) -> update (Eval x f df) bfgs

initState :: Control
          -> Vector Double
          -> (Double, Vector Double)
          -> State
initState c x0 (f0,g0)
    | any isNaN $ elemsVector x0 =
        error $ "component of initial position is NaN"
              ++ " (initial position is `" ++ show x0 ++ "')"        
    | isNaN f0 =
        error $ "initial value is NaN"
    | any isNaN $ elemsVector g0 =
        error $ "component of initial gradient is NaN"
              ++ " (initial gradient is `" ++ show g0 ++ "')"
    | dimVector x0 /= dimVector g0 =
        error $ "position dimension (`" ++ show (dimVector x0) ++ "')"
              ++ " and gradient dimension (`" ++ show (dimVector g0) ++ "')"
              ++ " do not match"
    | otherwise = let
        prev = Eval { position = x0, value = f0, gradient = g0 }
        cur = prev
        dir = negateVector g0
        ls0 = LineSearch.searchCont (linesearchControl c)
                                    (f0, g0 `dotVector` dir) 1
        (x,k) = case ls0 of
                    LineSearch.InProgress t k0 ->
                            (addVectorWithScales 1 x0 t dir, k0)
                    _ -> undefined
        in checkControl c $
               State { control = c
                     , iter = 0
                     , prevEval = prev
                     , curEval = cur
                     , invHessian = Nothing
                     , searchDir = dir
                     , searchPos = x
                     , linesearch = k
                     }

update :: Eval -> State -> MinimizeCont
update e bfgs
    | isNaN (value e) =
        error $ "function value is NaN"
    | any isNaN $ elemsVector (gradient e) =
        error $ "component of gradient is NaN"
              ++ " (gradient is `" ++ show (gradient e) ++ "')"
    | dimVector (position e) /= dimVector (gradient e) =
        error $ "position dimension (`"
              ++ show (dimVector $ position e) ++ "')"
              ++ " and gradient dimension (`"
              ++ show (dimVector $ gradient e) ++ "')"
              ++ " do not match"
    | (norm2Vector (gradient e)
           < gradTol (control bfgs) * 
                 max 1 (norm2Vector (position e))) =
        Converged
    | iter bfgs >= iterMax (control bfgs) =
        Stuck AtIterMax
    | otherwise =
        case unsafeUpdate e bfgs of
            Left w      -> Stuck (LineSearch w)
            Right bfgs' -> let
                x' = searchPos bfgs'
                in InProgress x' $ \(f',g') -> update (Eval x' f' g') bfgs'

unsafeUpdate :: Eval -> State -> Either LineSearch.Warning State
unsafeUpdate e bfgs = let
    dir = searchDir bfgs
    k = linesearch bfgs
    in case k (value e, gradient e `dotVector` dir) of
           LineSearch.Converged -> Right $ step e bfgs
           LineSearch.Stuck w   -> Left w
           LineSearch.InProgress t k' -> let
                x' = addVectorWithScales 1 (position (curEval bfgs))
                                         t (searchDir bfgs)
                in Right $ bfgs{ searchPos = x', linesearch = k' }

step :: Eval -> State -> State
step e bfgs = let
    dx = position e `subVector` position (curEval bfgs)
    dg = gradient e `subVector` gradient (curEval bfgs)
    dx_dg = dx `dotVector` dg
    dg_dg = dg `dotVector` dg
    hinv = case invHessian bfgs of
               Just hinv0 -> hinv0
               Nothing ->
                   let p = dimVector (position e)
                   in Herm Upper $
                          replaceMatrix (constantMatrix (p,p) 0)
                                        [ ((i,i), dx_dg / dg_dg)
                                        | i <- [ 0..p-1 ]
                                        ]
    hinv_dg = mulHermMatrixVector hinv dg
    dg_hinv_dg = hinv_dg `dotVector` dg
    hinv' = runHermMatrix $ do
                mhinv <- withHerm hinv $ \u a -> Herm u `fmap` newCopyMatrix a
                rank1UpdateToHermMatrix ((dg_hinv_dg/dx_dg+1)/dx_dg) dx mhinv
                rank2UpdateToHermMatrix (-1/dx_dg) dx hinv_dg mhinv
                return mhinv
    dir' = mulHermMatrixVectorWithScale (-1) hinv' (gradient e)
    ls = LineSearch.searchCont (linesearchControl $ control bfgs)
                               (value e, gradient e `dotVector` dir')
                               1
    (t',k') = case ls of LineSearch.InProgress t0 k0 -> (t0,k0)
                         _ -> undefined
                       
    x' = addVectorWithScales 1 (position e) t' dir'
    bfgs' = bfgs{ iter = iter bfgs + 1
                , prevEval = curEval bfgs
                , curEval = e
                , invHessian = Just hinv'
                , searchDir = dir'
                , searchPos = x'
                , linesearch = k'
                }
    in if (not . verbose) (control bfgs)
            then bfgs'
            else trace ("MINIMIZE iter: " ++ show (iter bfgs')
                        ++ " value: " ++ show (value e)
                        ++ " dec: "
                        ++   show (-(gradient e `dotVector` searchDir bfgs')/2)
                        ++ " step: " ++   show (norm2Vector dx)
                        ) bfgs'


-- | The result of a minimization.
data Result a =
    Result { evalCount   :: !Int             -- ^ number of function evalutations
           , resultPos   :: !(Vector Double) -- ^ position
           , resultValue :: !Double          -- ^ function value
           , resultGrad  :: !(Vector Double) -- ^ function gradient
           , resultState :: a                -- ^ extra state (lazy)
           }
    deriving (Show)

-- | Minimize a differentiable function.  Upon success return 'Right' and the
-- (local) optimum; upon failure return 'Left' with a warning and the best
-- solution obtained during the minimization.
minimize :: Control
         -- ^  minimization parameters
         -> (Vector Double -> (Double, Vector Double, a))
         -- ^ function mapping position to value, gradient, and extra state
         -> Vector Double
         -- ^ initial position
         -> (Double, Vector Double, a)
         -- ^ initial, value, gradient, and extra state
         -> Either (Warning, Result a) (Result a)
         -- ^ minimization result
minimize c fdf x0 (f0,df0,a0) =
    runIdentity $ minimizeM c (return . fdf) x0 (f0,df0,a0)

newtype Identity a = Identity { runIdentity :: a }
instance Monad Identity where
    return = Identity
    {-# INLINE return #-}
    m >>= f = f $ runIdentity m
    {-# INLINE (>>=) #-}


-- | Monadic version of 'minimize'.
minimizeM :: (Monad m)
          => Control
          -- ^ minimization parameters
          -> (Vector Double -> m (Double, Vector Double, a))
          -- ^ function mapping position to value, gradient, and extra state
          -> Vector Double
          -- ^ initial position
          -> (Double, Vector Double, a)
          -- ^ initial, value, gradient, and extra state
          -> m (Either (Warning, Result a) (Result a))
          -- ^ minimization result
{-# INLINE minimizeM #-}
minimizeM c fdf x0 (f0,df0,a0) =
    convergeM fdf 0 (Eval x0 f0 df0, a0) $
        minimizeCont c x0 (f0,df0)

{-# INLINE convergeM #-}
convergeM :: (Monad m)
          => (Vector Double -> m (Double, Vector Double, a))
          -> Int
          -> (Eval, a)
          -> MinimizeCont
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
    result = Result { evalCount = i
                    , resultPos = position e
                    , resultValue = value e
                    , resultGrad = gradient e
                    , resultState = a 
                    }

-- $references
--
-- * Broyden, C. G. (1970) The convergence of a class of double-rank
--   minimization algorithms.
--   /Journal of the Institute of Mathematics and its Applications/
--   6(1): 76&#8211;90. <http://dx.doi.org/10.1093/imamat/6.1.76>
--
-- * Fletcher, R. (1970) A new approach to variable metric algorithms.
--   /The Computer Journal/ 13(3): 317&#8211;322.
--   <http://dx.doi.org/10.1093/comjnl/13.3.317>
--
-- * Goldfarb, D. (1970) A family of variable metric updates derived
--   by variational means. /Mathematics of Computation/ 24(109): 23&#8211;26.
--   <http://dx.doi.org/10.1090/S0025-5718-1970-0258249-6>
--
-- * Nocedal, J. and Wright, S. J. (2006) /Numerical Optimization/, 2nd ed.
--   Springer. <http://www.springer.com/mathematics/book/978-0-387-30303-1>
--
-- * Shanno, D. F. (1970) Conditioning of quasi-Newton methods for function
--   minimization. /Mathematics of Compututaiton/ 24(111): 647&#8211;656.
--   <http://dx.doi.org/10.1090/S0025-5718-1970-0274029-X>
--