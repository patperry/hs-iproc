module Main
    where
        
import Data.Ratio( approxRational, (%) )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified LineSearch as LineSearch





tests_LineSearch = testGroup "LineSearch"
    [ test_linesearch1
    , test_linesearch2
    , test_linesearch3
    , test_linesearch4
    , test_linesearch5
    , test_linesearch6
    ]

testSearch name control phi cases =
    testGroup name
        [ testGroup (show alpha0)
              [ testCase "alpha" $
                    case LineSearch.search control phi alpha0 of
                        Right e -> trunc2 (LineSearch.position e) @?= alpha
                        Left e -> assertFailure $ "Warning: " ++ show e
              , testCase "dphi" $
                    case LineSearch.search control phi alpha0 of
                        Right e -> trunc2 (LineSearch.deriv e) @?= dphi
                        Left e -> assertFailure $ "Warning: " ++ show e
              ]
        | (alpha0,m,alpha,dphi) <- cases
        ]

trunc2 :: Double -> Double
trunc2 x | x < 0 = negate (trunc2 $ abs x)
         | otherwise =
    if e <= 1
        then fromRational $
                ( (round . (10^(-e+1) *)) x
                % 10^(-e+1) )
        else
            (fromIntegral . round $ x / 10^(e-1)) * 10^(e-1)
  where
    e = floor $ logBase 10 x :: Int



test_linesearch1 = testSearch "Table I" control1 phi1 $
        [ (1e-3, 6,  1.4, -9.2e-3)
        , (1e-1, 3,  1.4,  4.7e-3)
        , (1e+1, 1, 10  ,  9.4e-3)
        , (1e+3, 4, 37  ,  7.3e-4)
        ]

control1 = LineSearch.defaultControl { LineSearch.valueTol = 0.001
                                     , LineSearch.derivTol = 0.1   }
phi1 alpha =
    ( -alpha/(alpha^^2 + beta)
    ,  (alpha^^2 - beta)/(alpha^^2 + beta)^^2
    , ()
    )
  where
    beta = 2


test_linesearch2 = testSearch "Table II" control2 phi2 $
        [ (1e-3, 12, 1.6,  7.1e-9)
        , (1e-1,  8, 1.6,    1e-10) -- (reported as 10e-10 in paper; typo?)
        , (1e+1,  8, 1.6, -5.0e-9)
        , (1e+3, 11, 1.6, -2.3e-8)
        ]

control2 = LineSearch.defaultControl { LineSearch.valueTol = 0.1
                                     , LineSearch.derivTol = 0.1
                                     , LineSearch.stepTol = 1e-5 }

phi2 alpha =
    ( (alpha + beta)^^5  - 2 * (alpha + beta)^^4
    , 5 * (alpha + beta)^^4  - 8 * (alpha + beta)^^3
    , ()
    )
  where
    beta = 0.004


test_linesearch3 = testSearch "Table III" control3 phi3 $
        [ (1e-3, 12, 1.0, -5.1e-5)
        , (1e-1, 12, 1.0, -1.9e-4)
        , (1e+1, 10, 1.0, -2.0e-6)
        , (1e+3, 13, 1.0, -1.6e-5)
        ]

control3 = LineSearch.defaultControl { LineSearch.valueTol = 0.1
                                     , LineSearch.derivTol = 0.1
                                     , LineSearch.stepTol = 1e-5 }

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


test_linesearch4 = testSearch "Table IV" control4 phi4 $
        [ (1e-3, 4, 0.085, -6.9e-5) -- reported as 0.08 in paper
        , (1e-1, 1, 0.10, -4.9e-5)
        , (1e+1, 3, 0.35, -2.9e-6)
        , (1e+3, 4, 0.83,  1.6e-5)
        ]

test_linesearch5 = testSearch "Table V" control5 phi5 $
        [ (1e-3, 6, 0.075,  1.9e-4)
        , (1e-1, 3, 0.078,  7.4e-4)
        , (1e+1, 7, 0.073, -2.6e-4)
        , (1e+3, 8, 0.076,  4.5e-4)
        ]

test_linesearch6 = testSearch "Table VI" control6 phi6 $
        [ (1e-3, 13, 0.93,  5.2e-4)
        , (1e-1, 11, 0.93,  8.4e-5)
        , (1e+1,  8, 0.92, -2.4e-4)
        , (1e+3, 11, 0.92, -3.2e-4)
        ]

control4 = LineSearch.defaultControl { LineSearch.valueTol = 0.001
                                     , LineSearch.derivTol = 0.001
                                     , LineSearch.stepTol = 1e-5 }
control5 = control4
control6 = control4
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


main :: IO ()
main = defaultMain [ tests_LineSearch
                   ]
