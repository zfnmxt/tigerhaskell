module DFA where

import Data.Graph (Vertex)
import qualified Data.Graph as G
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Prelude hiding (lookup)

data DFAState p a b = DFAState
  { stateId :: Int,
    transition :: a -> Int,
    construct :: Maybe ([a] -> b),
    payload :: p
  }

instance Show (DFAState s a b) where
  show (DFAState num _ _ _) = "DFAState (" ++ show num ++ ")"

data DFA p a b = DFA
  { dfaStates :: Map Int (DFAState p a b),
    startId :: Int,
    endIds :: Set Int
  }

type Id = Int

invert :: (Ord k, Ord v) => Map k v -> Map v k
invert m = M.fromListWith (error "Malconstructed DFA") [(v, k) | (k, v) <- M.toList m]

lookup :: DFA p a b -> Int -> DFAState p a b
lookup dfa s = case dfaStates dfa M.!? s of
  Nothing -> error $ "lookup: " ++ show s
  Just st -> st

lookupPayload :: DFA p a b -> Int -> p
lookupPayload dfa = payload . lookup dfa

lookupId :: (Ord p, Eq p) => DFA p a b -> p -> Maybe Int
lookupId dfa = (m_payload M.!?)
  where
    m_payload = invert $ M.map payload $ dfaStates dfa

insert :: DFA p a b -> Int -> DFAState p a b -> DFA p a b
insert dfa s ds = dfa {dfaStates = M.insert s ds $ dfaStates dfa}

update :: DFA p a b -> Int -> (DFAState p a b -> DFAState p a b) -> DFA p a b
update dfa s f = dfa {dfaStates = M.insert s (f (lookup dfa s)) (dfaStates dfa)}

updateTrans :: DFA p a b -> Int -> ((a -> Int) -> (a -> Int)) -> DFA p a b
updateTrans dfa s f = update dfa s (\s -> s {transition = f $ transition s})

matches :: DFA p a b -> [a] -> Bool
matches dfa as = matches' (startId dfa) as
  where
    matches' s [] = s `S.member` endIds dfa
    matches' s (a : as) =
      let state = dfaStates dfa M.! s
          s' = transition state a
       in matches' s' as

minimize :: DFA p a b -> DFA p a b
minimize = undefined
  where
    inequivalent :: DFA p a b -> Set a -> Int -> Int -> Bool
    inequivalent dfa sigma x y
      | x `S.member` endIds dfa /= y `S.member` endIds dfa = True
      | otherwise = undefined

data Graph a = Graph
  { graph :: G.Graph,
    nodeFromVertex :: Vertex -> ((), (a, Id), [(a, Id)]),
    vertexFromKey :: (a, Id) -> Maybe Vertex,
    startIdG :: Id,
    endIdsG :: Set Id
  }

instance Show (Graph a) where
  show (Graph g _ _ start ends) = "(" ++ show g ++ "," ++ show start ++ "," ++ show ends ++ ")"

toGraph :: forall p a b. Ord a => DFA p a b -> Set a -> Graph a
toGraph dfa sigma =
  let (g, nfv, vfk) = G.graphFromEdges edges
   in Graph
        { graph = g,
          nodeFromVertex = nfv,
          vertexFromKey = vfk,
          startIdG = startId dfa,
          endIdsG = endIds dfa
        }
  where
    ss = dfaStates dfa
    ts = M.map transition ss
    edges = M.foldr f [] ss
    f :: DFAState p a b -> [((), (a, Id), [(a, Id)])] -> [((), (a, Id), [(a, Id)])]
    f s = ((map (\a -> ((), (a, stateId s), [(a, transition s a)])) $ S.toList sigma) ++)

matchesGraph :: Graph a -> [a] -> Bool
matchesGraph g as = matches' (startIdG g) as
  where
    matches' id [] = id `S.member` endIdsG g
    matches' id (a : as) =
      case vertexFromKey g (a, id) of
        Nothing -> error "Malformed graph"
        Just v ->
          let (_, _, [(_, id')]) = nodeFromVertex g v
           in matches' id' as
