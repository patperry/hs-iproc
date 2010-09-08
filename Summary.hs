module Summary (
    Summary,
    empty,
    insert,
    fromMessages,
    
    vars,
    varsSum,
    counts,
    ) where

import Data.List( foldl' )
import Data.Map( Map )
import qualified Data.Map as Map
import Numeric.LinearAlgebra

import History( History )
import qualified History as History
import Types
import Vars( Vars )
import qualified Vars as Vars

unionWith' :: (Ord k) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith' f m m' =
    foldl' (flip $ uncurry $ Map.insertWith' (flip f))
           m
           (Map.toList m')

        
data SenderSummary =
    SenderSummary { senderVars :: !Vars
                  , sender :: !SenderId
                  , sendCount :: !Int
                  , receiveCount :: !(Map ReceiverId Int)
                  , observedVarChanges :: !(Map Int Double)
                  }
    deriving (Show)

varsSumSS :: SenderSummary -> Vector Double
varsSumSS ss = let
    rws = [ (r, fromIntegral w) | (r,w) <- Map.assocs rc ]
    in accumVector (+) (Vars.weightReceiverBy rws v h0 s) (Map.assocs ovc)
  where
    v = senderVars ss
    s = sender ss
    rc = receiveCount ss
    ovc = observedVarChanges ss
    h0 = History.empty

emptySS :: Vars -> SenderId -> SenderSummary
emptySS v s = SenderSummary v s 0 Map.empty Map.empty

singletonSS :: Vars -> SenderId -> ([ReceiverId], History) -> SenderSummary
singletonSS v s (rs,h) = let
    sc = length rs
    rc = Map.fromList $ zip rs (repeat 1)
    ovc = foldl' (flip $ uncurry $ Map.insertWith' (+)) Map.empty $
              concatMap (Vars.dyadChanges v h s) rs
    in SenderSummary v s sc rc ovc
    
unionSS :: SenderSummary -> SenderSummary -> SenderSummary
unionSS (SenderSummary v s sc1 rc1 ovc1)
        (SenderSummary _ _ sc2 rc2 ovc2) =
            SenderSummary v s
                          ((+) sc1 sc2)
                          (unionWith' (+) rc1 rc2)
                          (unionWith' (+) ovc1 ovc2)

insertSS :: ([ReceiverId], History) -> SenderSummary -> SenderSummary
insertSS msg ss =
    unionSS ss $ singletonSS (senderVars ss) (sender ss) msg

data Summary = Summary { vars :: !Vars
                       , count :: !Int
                       , senderSummary :: !(Map SenderId SenderSummary)
                       }
    deriving (Show)

varsSum :: Summary -> Vector Double
varsSum s =
    sumVector (Vars.dim $ vars s)
              (map varsSumSS (Map.elems $ senderSummary s))

counts :: Summary -> [((SenderId,ReceiverId), Int)]
counts smry = concat
    [ [ ((s,r),c) | (r,c) <- Map.assocs (receiveCount ss) ]
    | (s,ss) <- Map.assocs (senderSummary smry)
    ]

empty :: Vars -> Summary
empty v = Summary v 0 Map.empty

singleton :: Vars -> (Message, History) -> Summary
singleton v (Message s rs, h) =
    Summary v (length rs) $ Map.singleton s $ singletonSS v s (rs,h)

union :: Summary -> Summary -> Summary
union (Summary v c1 ss1) (Summary _ c2 ss2) =
    Summary v (c1 + c2) (unionWith' unionSS ss1 ss2)

insert :: (Message, History) -> Summary -> Summary
insert msg s =
    union s $ singleton (vars s) msg

fromMessages :: Vars -> [(Message, History)] -> Summary
fromMessages v mhs =
    foldl' (flip insert) (empty v) mhs
