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


data Control =
    Control {
          linesearchControl :: !LineSearch.Control
                                    -- ^ control parameters for the line
                                    --   search
        , verbose :: !Bool          -- ^ indicates whether to print status
                                    --   to stderr after each iterate
                                    --   (default @False@)
        } deriving (Show)
  
defaultControl :: Control
defaultControl =
    Control { linesearchControl = LineSearch.defaultControl
            , verbose = True
            }
  
checkControl :: Control -> a -> a
checkControl _c
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

data Warning = NoWarning

data MinimizeCont = Converged
                  | Stuck Warning
                  | InProgress !(Vector Double)
                               !((Double, Vector Double) -> MinimizeCont)

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
    bfgs = initState c x0 fdf0
    x = searchPos bfgs
    in InProgress x $ \(f,df) -> update (Eval x f df) bfgs

initState :: Control
          -> Vector Double
          -> (Double, Vector Double)
          -> State
initState c x0 (f0,g0)
    | any isNaN $ elemsVector x0 =
        error $ "component of initial position is NaN"
              ++ " (initial position is is `" ++ show x0 ++ "')"        
    | isNaN f0 =
        error $ "initial value is NaN"
    | any isNaN $ elemsVector g0 =
        error $ "component of initial gradient is NaN"
              ++ " (initial gradient is is `" ++ show g0 ++ "')"
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
update e bfgs = let
    -- TODO: check for convergence
    bfgs' = unsafeUpdate e bfgs
    x' = searchPos bfgs'
    in InProgress x' $ \(f',g') -> update (Eval x' f' g') bfgs'


unsafeUpdate :: Eval -> State -> State
unsafeUpdate e bfgs = let
    it' = iter bfgs + 1
    dir = searchDir bfgs
    k = linesearch bfgs
    bfgs' =
        case k (value e, gradient e `dotVector` dir) of
            LineSearch.Converged -> step e bfgs
            LineSearch.Stuck _w  -> step e bfgs  
            LineSearch.InProgress t k' -> let
                x' = addVectorWithScales 1 (position (curEval bfgs))
                                         t (searchDir bfgs)
                in bfgs{ searchPos = x', linesearch = k' }
    in bfgs'{ iter = it' }
    
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
                rank1UpdateToHermMatrix ((dg_hinv_dg/dx_dg+1)/dx_dg) dg mhinv
                rank2UpdateToHermMatrix (-1/dx_dg) dx hinv_dg mhinv
                return mhinv
    dir' = mulHermMatrixVectorWithScale (-1) hinv' (gradient e)
    ls = LineSearch.searchCont (linesearchControl $ control bfgs)
                               (value e, gradient e `dotVector` dir')
                               1
    (t',k') = case ls of LineSearch.InProgress t0 k0 -> (t0,k0)
                         _ -> undefined
                       
    x' = addVectorWithScales 1 (position e) t' dir'
    in bfgs{ prevEval = curEval bfgs
           , curEval = e
           , invHessian = Just hinv'
           , searchDir = dir'
           , searchPos = x'
           , linesearch = k'
           }

-- | The result of a line search.
data Result a =
    Result { resultIter  :: !Int             -- ^ iterate number
           , resultPos   :: !(Vector Double) -- ^ position
           , resultValue :: !Double          -- ^ function value
           , resultGrad  :: !(Vector Double) -- ^ function gradient
           , resultState :: a                -- ^ extra state (lazy)
           }
    deriving (Eq, Show)

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
    result = Result { resultIter = i
                    , resultPos = position e
                    , resultValue = value e
                    , resultGrad = gradient e
                    , resultState = a 
                    }

-- $references
--
-- * Nocedal, J. and Wright, S. J. (2006) /Numerical Optimization/, 2nd ed.
--   Springer. <http://www.springer.com/mathematics/book/978-0-387-30303-1>
--
