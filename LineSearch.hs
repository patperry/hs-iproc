module LineSearch
    where
        
data Control = 
    Control { funTol :: !Double
            , gradTol :: !Double
            , stepTol :: !Double
            , stepMin :: !Double
            , stepMax :: !Double
            }
    deriving Show

defaultControl :: Control
defaultControl =
    Control { funTol = 1e-3
            , gradTol = 0.9
            , stepTol = 0.1
            , stepMin = 0
            , stepMax = 1e10
            }

checkControl :: Control -> a -> a
checkControl c
    | not (funTol c >= 0) =
        error $ "invalid funTol: `" ++ show (funTol c) ++ "'"
    | not (gradTol c >= 0) =
        error $ "invalid gradTol: `" ++ show (gradTol c) ++ "'"
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
         , value :: !Double
         , deriv :: !Double
         , state :: a
         }
    deriving Show

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
    

init :: Control -> Function a -> Double -> LineSearch a
init c f x0
    | not (stepMin c <= x0) =
          error $ "invalid step: `" ++ show x0 ++ "'"
                ++ " (stepMin is `" ++ show (stepMin c) ++ "')"
    | not (x0 <= stepMax c) =
          error $ "invalid step: `" ++ show x0 ++ "'"
                ++ " (stepMax is `" ++ show (stepMax c) ++ "')"
    | otherwise =
        let
            (f0,d0,a0) = f x0
            gtest = d0 * funTol c
            ftest = f0 + x0 * gtest
            w = stepMax c - stepMin c
            eval = Eval { position = x0, value = f0, deriv = d0, state = a0 }
        in
            checkControl c $
                LineSearch { control = c
                           , function = f
                           , value0 = f0
                           , deriv0 = d0
                           , valueTest = ftest
                           , derivTest = gtest
                           , bracketed = False
                           , stage1 = True
                           , lowerEval = eval
                           , upperEval = eval
                           , testEval = eval
                           , stepLower = 0
                           , stepUpper = x0 + 4.0 * x0
                           , width = w
                           , width' = 2 * w
                           }

data Warning = RoundErr | StepTol | AtStepMax | AtStepMin
data Status a = Converged (Eval a)
              | Warning (Eval a)
              | InProgress

status :: LineSearch a -> Status a
statux ls


step :: LineSearch a -> LineSearch a
step ls = let
    (lower, upper, test) = (lowerEval ls, upperEval ls, testEval ls)
    tbounds = (stepLower ls, stepUpper ls)
    ftest = valueTest ls
    gtest = derivTest ls
    
    -- if psi(t) <= 0 and f'(t) >= 0 for some step, then the
    -- algorithm enters the second stage
    
    stg1' = if not (stage1 ls)
                then False
                else value test <= ftest && deriv test >= 0
                 
    -- A modified function is used to predict the step during the
    -- first stage if a lower function value has been obtained but
    -- the decrease is not sufficient.
    modify = stg1' && value test <= value lower && value test > ftest
    (mlower, mupper, mtest) =
        if modify
            then ( lower{ value = value lower - position lower * gtest
                        , deriv = deriv lower - gtest
                        }
                 , upper{ value = value upper - position upper * gtest
                        , deriv = deriv upper - gtest
                        }
                 , test{ value = value test - position test * gtest
                       , deriv = deriv test - gtest
                       }
                 )
            else (lower, upper, test)
    
    -- compute new step
    (brackt', t0') = trialValue tbounds (bracketed ls) (mlower, mupper) mtest
    
    -- update bounds
    (mlower', mupper') = updateInterval (mlower,mupper) mtest
    
    -- Restore function and derivative values
    (lower', upper') =
        if modify
            then ( mlower'{ value = value mlower' + position mlower' * gtest
                          , deriv = deriv mlower' + gtest
                          }
                 , mupper'{ value = value mupper' + position mupper' * gtest
                          , deriv = deriv mupper' + gtest
                          }
                )
            else (mlower', mupper')
    
    -- perform a bisection step if necessary
    (t1',w',w'') =
        if brackt'
            then ( position lower' + 0.5 * (position upper' - position lower')
                 , abs (position upper' - position lower')
                 , width ls
                 )
            else ( t0', width ls, width' ls )

    -- set the minimum and maximum steps allowed
    (tmin',tmax') =
        if brackt'
            then ( min (position lower') (position upper')
                 , max (position lower') (position upper')
                 )
            else ( t1' + 1.1 * (t1' - position lower')
                 , t1' + 4.0 * (t1' - position upper')
                 )
    
    -- force the step to be within bounds
    t' = (max tmin' . min tmax') t1'

    -- if further progress is not possible, take step to be the best
    -- point obtained so far
    test' = if brackt' && (  t' <= tmin'
                          || t' >= tmax'
                          || tmax' - tmin' <= stepTol (control ls) * tmax'
                          )
                then lower'
                else let (f,g,a) = (function ls) t' in Eval t' f g a
    ftest' = value0 ls + position test' * gtest
    
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
    | brackt && not (gl * (t - l) > 0) =
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
            if abs (c - t) >= abs (q - t)
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

               -- extrapolate to farthest of cubic and secand steps
               False | abs (t - c') > abs (t - s) -> clip c'
               False | otherwise                  -> clip s
       
    -- Case 4: lower function value, derivatives same sign, higher derivative
    | otherwise =
        result brackt $
            case brackt of
                True              -> cubicMin (t,ft,gt) (u,fu,gu)
                False | l < t     -> tmin
                False | otherwise -> tmax
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
	in u + du / ((fu - fv) / a + du) / 2 * a
	
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
