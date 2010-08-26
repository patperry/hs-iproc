module LineSearch (
    Control(..),
    defaultControl,
    
    Function,
    Eval(..),
    Warning(..),
    search,
    
    LineSearch,
    Status(..),
    init,
    step,
    best,
    converge,
    ) where

import Prelude hiding ( init )

phi1 alpha =
    ( -alpha/(alpha^^2 + beta)
    ,  (alpha^^2 - beta)/(alpha^^2 + beta)^^2
    , ()
    )
  where
    beta = 2

phi2 alpha =
    ( (alpha + beta)^^5  - 2 * (alpha + beta)^^4
    , 5 * (alpha + beta)^^4  - 8 * (alpha + beta)^^3
    , ()
    )
  where
    beta = 0.004

phi3 alpha = let
    (f0,g0,_) = phi0
    in ( f0 + 2 * (1 - beta)/( l * pi ) * sin (l * pi / 2 * alpha)
       , g0 + (1 - beta) * cos (l * pi / 2 * alpha)
       , ()
       )
  where
    beta = 0.01
    l = 39
    phi0  =
        ( if alpha <= 1 - beta
                then 1 - alpha
                else if alpha >= 1 + beta
                    then alpha - 1
                    else (alpha - 1)^^2 /( 2 * beta ) + 0.5 * beta
        , if alpha <= 1 - beta
                then -1
                else if alpha >= 1 + beta
                    then 1
                    else (alpha - 1) / beta
        , ()
        )

phi4 = yanai 0.001 0.001
phi5 = yanai 0.01 0.001
phi6 = yanai 0.001 0.01

yanai beta1 beta2 alpha =
    ( gamma beta1 * sqrt( (1 - alpha)^^2 + beta2^^2 )
      + gamma beta2 * sqrt( alpha^^2 + beta1^^2 )
    , gamma beta1 * (alpha - 1) / sqrt( (1 - alpha)^^2 + beta2^^2 )
      + gamma beta2 * alpha / sqrt( alpha^^2 + beta1^^2 )
    , ()
    )
  where
    gamma beta = sqrt (1 + beta^^2) - beta
    

data Control = 
    Control { valueTol :: !Double
            , derivTol :: !Double
            , stepTol :: !Double
            , stepMin :: !Double
            , stepMax :: !Double
            }
    deriving Show

defaultControl :: Control
defaultControl =
    Control { valueTol = 1e-3
            , derivTol = 0.9
            , stepTol = 0.1
            , stepMin = 0
            , stepMax = 1e10
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
    | otherwise = id



type Function a = Double -> (Double, Double, a)

data Eval a = 
    Eval { position :: !Double
         , value :: Double  -- lazy in case
         , deriv :: Double  --              we have converged
         , state :: a
         }
    deriving Show

data LineSearch a =
    LineSearch { control :: !Control
               , function :: !(Function a)
               , valueTest :: Double -- lazy in case we have converged
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

best :: LineSearch a -> Eval a
best = lowerEval
    
search :: Control
       -> Function a
       -> Double
       -> Either (Warning, Eval a) (Eval a)
search c f step0 = converge $ init c f step0

converge :: LineSearch a -> Either (Warning, Eval a) (Eval a)
converge ls =
    case step ls of
        Stuck w e      -> Left (w,e)
        Converged e    -> Right e
        InProgress ls' -> converge ls'

init :: Control -> Function a -> Double -> LineSearch a
init c fdf step
    | not (stepMin c <= step) =
          error $ "invalid step: `" ++ show step ++ "'"
                ++ " (stepMin is `" ++ show (stepMin c) ++ "')"
    | not (step <= stepMax c) =
          error $ "invalid step: `" ++ show step ++ "'"
                ++ " (stepMax is `" ++ show (stepMax c) ++ "')"
    | otherwise =
        let
            (f0,d0,a0) = fdf 0
            (f,d,a) = fdf step
            gtest = d0 * valueTol c
            ftest = f0 + step * gtest
            w = stepMax c - stepMin c
            lower = Eval { position = 0, value = f0, deriv = d0, state = a0 }
            upper = lower
            test = Eval { position = step, value = f, deriv = d, state = a}
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
                           , stepUpper = step + 4.0 * step
                           , width = w
                           , width' = 2 * w
                           }

data Warning = RoundErr | WithinTol | AtStepMax | AtStepMin
    deriving (Eq, Show)
data Status a = Converged (Eval a)
              | Stuck Warning (Eval a)
              | InProgress (LineSearch a)

step :: LineSearch a -> Status a
step ls
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
    (modify, reset) =
        if stg1' && value test <= value lower && value test > ftest
              then ( \e -> e{ value = value e - position e * gtest
                            , deriv = deriv e - gtest }
                   , \e -> e{ value = value e + position e * gtest
                            , deriv = deriv e + gtest }
                   )
              else (id, id)
    (mlower, mupper, mtest) = (modify lower, modify upper, modify test)
    
    -- compute new step and update bounds
    (brackt', t0') = trialValue (stepLower ls, stepUpper ls) (bracketed ls)
                                (mlower, mupper) mtest
    (mlower', mupper') = updateInterval (mlower,mupper) mtest
    (lower', upper') = (reset mlower', reset mupper')
    
    -- perform a bisection step if necessary
    (w',w'',t1') =
        if brackt'
            then ( abs (position upper' - position lower')
                 , width ls
                 , if w' >= 0.66 * width' ls
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
            else ( t1' + 1.1 * (t1' - position lower')
                 , t1' + 4.0 * (t1' - position lower')
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
updateInterval :: (Eval a, Eval a)
               -> Eval a
               -> (Eval a, Eval a)
updateInterval (lower@(Eval l fl gl _), upper@(Eval u fu gu _))
               test@(Eval t ft gt _)
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
trialValue :: (Double, Double)
           -> Bool
           -> (Eval a, Eval a)
           -> Eval a
           -> (Bool, Double)
trialValue (tmin,tmax)
           brackt
           (lower@(Eval l fl gl _), upper@(Eval u fu gu _))
           test@(Eval t ft gt _)
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
               True | abs (t - c') < abs (t - s) -> safegaurd c'
               True | otherwise                  -> safegaurd s

               -- extrapolate to farthest of cubic and secant steps
               False | abs (t - c') > abs (t - s) -> clip c'
               False | otherwise                  -> clip s
       
    -- Case 4: lower function value, derivatives same sign, higher derivative
    | otherwise =
        result brackt $
            case brackt of
                True              -> cubicMin (t,ft,gt) (u,fu,gu)
                False | l < t     -> tmax
                False | otherwise -> tmin
  where
    c = cubicMin (l,fl,gl) (t,ft,gt)
    q = quadrMin (l,fl,gl) (t,ft)
    s = secant (l,gl) (t,gt)

    clip = max tmin . min tmax

    safegaurd | t > l     = min (t + 0.66 * (u - t))
              | otherwise = max (t + 0.66 * (u - t))

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
