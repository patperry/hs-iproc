{-# LANGUAGE ForeignFunctionInterface #-}
module Bootstrap (
    fromSends
    ) where

import Control.Monad( replicateM )
import Control.Monad.MC( MC, replicateMC, sampleWithWeights,
    sampleSubsetWithWeights, unsafeInterleaveMC )
import Data.List( nub )
import Debug.Trace

import History( History )
import Model( Model )
import Types( SenderId, Message(..) )

import qualified Model as Model

type LogWeight = Double

fromSends :: Model
          -> [((SenderId, Int), History)]
          -> MC (LogWeight, [(Message, History)])
fromSends m = go 0
  where
    go acc [] = return (acc, [])
    go acc (((s,l),h):slhs) = let
        model_rps = Model.probs m h s
        sampleWithReplace = sampleWithWeights [ (p,r) | (r,p) <- model_rps ]
        ntry = 100000
        in do
            mrs <- tryRejection sampleWithReplace l ntry
            case mrs of
                Just rs ->
                    let msg = Message s rs
                    in do
                       (lw,mhs) <- go acc slhs
                       msg `seq` return $ (lw, (msg,h):mhs)
                Nothing ->
                    let prps = [ (p, (r,p)) | (r,p) <- model_rps ]
                    in unsafeInterleaveMC $ do
                        rps <- sampleSubsetWithWeights prps l
                        let (rs,ps) = unzip rps
                            msg     = Message s rs
                            acc'    = acc + logWeight ps 0 0
                        (lw,mhs) <- acc' `seq` go acc' slhs
                        msg `seq` return $ 
                            trace ("l: " ++ show l ++ " acc: " ++ show acc ++ " lw: " ++ show (acc' - acc) ++ " ps: " ++ show ps)
                                  (lw, (msg,h):mhs)

    logWeight ps tot acc = case ps of
        []      -> acc
        (p:ps') -> let tot' = tot + p
                       acc' = acc + log1p (-tot)
                   in acc' `seq` tot' `seq` logWeight ps' tot' acc'

    tryRejection sample l ntry | ntry == 0 = return Nothing
                               | l == 1    = (Just . (:[])) `fmap` sample
                               | otherwise = do
        rs <- replicateMC l sample
        if (length . nub) rs == l
             then return $ Just rs
             else tryRejection sample l (ntry-1)

log1p :: Double -> Double
log1p x = log (1 + x)

-- foreign import ccall unsafe "log1p"
--    log1p :: Double -> Double

