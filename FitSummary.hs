module FitSummary (
    hPutFitSummary,
    hPutCovSummary,
    ) where

import Control.Monad
import System.IO

import Numeric.LinearAlgebra

import Fit( Result )
import LogLik( LogLik )
import Types

import qualified Fisher as Fisher
import qualified Fit as Fit
import qualified Intervals as Intervals
import qualified LogLik as LogLik
import qualified Model as Model
import qualified Vars as Vars

hPutFitSummary :: Handle -> (Double, Result LogLik) -> IO ()
hPutFitSummary h (penalty,r) = let
    ll = Fit.resultState r
    m = LogLik.model ll
    v = Model.vars m
    in do
        hPutScalar h "penalty" $ penalty
        hPutList h "intervals.send" $ 
            map toInt $ Intervals.toList $ Vars.sendIntervals v
        hPutList h "intervals.recv" $
            map toInt $ Intervals.toList $ Vars.receiveIntervals v
        hPutScalar h "dev.null" $ LogLik.nullDeviance ll
        hPutScalar h "df.null" $ LogLik.nullDf ll
        hPutScalar h "dev.resid" $ LogLik.deviance ll
        hPutScalar h "df.resid" $ LogLik.residDf ll
        hPutScalar h "df" $ LogLik.nullDf ll - LogLik.residDf ll
        hPutVector h "coefs" $ Model.coefs $ LogLik.model ll
  where
    toInt :: DiffTime -> Int
    toInt = fromIntegral

-- fish = Fisher.fromMessages m mhs

hPutCovSummary :: Handle -> Herm Matrix Double -> IO ()
hPutCovSummary h fish = let
    tol = 1e-6
    (fishinv,nzero) = Fisher.invWithTol tol fish
    (se, n) = withHerm fishinv $ \_ a -> 
                      ( sqrtVector $ diagMatrix a
                      , fst $ dimMatrix a )

    in do
        hPutScalar h "df.effective" $ n - nzero
        hPutVector h "coefs.se" se
        withHerm fishinv $ \_ a ->
            hPutMatrix h "coefs.cov" a


hPutScalar :: (Show a) => Handle -> String -> a -> IO ()
hPutScalar h name a = do
    hPutStrLn h $ name ++ " <- " ++ show a

hPutMatrix :: (Show a, Storable a) => Handle -> String -> Matrix a -> IO ()
hPutMatrix h name a = do
    hPutStr h $ name ++ " <- matrix("
    hPutRawList h $ elemsMatrix a
    hPutStr h $ ", nrow = " ++ show m ++ ", ncol = " ++ show n ++ ")\n"
  where
    (m,n) = dimMatrix a

hPutVector :: (Show a, Storable a) => Handle -> String -> Vector a -> IO ()
hPutVector h name x =
    hPutList h name $ elemsVector x

hPutList :: (Show a) => Handle -> String -> [a] -> IO ()
hPutList h name xs = do
    hPutStr h $ name ++ " <- "
    hPutRawList h xs
    hPutStrLn h ""

hPutRawList :: (Show a) => Handle -> [a] -> IO ()
hPutRawList h xs = do
    hPutStr h "c("
    when ((not . null) xs) $ do
        hPutStr h (show $ head xs)
        sequence_ [ hPutStr h $ ", " ++ show x | x <- tail xs ]
    hPutStr h $ ")"
