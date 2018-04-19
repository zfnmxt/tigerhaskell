{- Implements Algorithm 3.13
   Grammar rules must be specified as "X -> XacY"
   where uppercase letters are nonterminals and lowercase are terminals.
   Each line can only contain one rule.
-}

module FirstFollow where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Control.Monad
import Control.Monad.Trans.State as S
import Text.ParserCombinators.ReadP
import Data.Char (isUpper, isLower, isAlpha)
import System.IO
import Data.Maybe (fromMaybe)
import Data.List  (subsequences, tails, inits)

newtype Terminal    = Terminal Char deriving (Show, Eq, Ord)
newtype NonTerminal = NonTerminal Char deriving (Show, Eq, Ord)
type Symbol         = Either Terminal NonTerminal
type Nullable       = Set.Set NonTerminal
type FirstSet       = Set.Set Terminal
type FollowSet      = Set.Set Terminal
data Rule           = Rule NonTerminal [Symbol] deriving (Show, Eq)
data Env            = Env { rules       :: Map.Map NonTerminal [Rule]
                          , nullableSet :: Nullable
                          , firstMap    :: Map.Map Symbol FirstSet
                          , followMap   :: Map.Map Symbol FollowSet
                          } deriving (Show, Eq)

fileName = "grammar.txt"

testRules = [Rule (NonTerminal 'X') [Left (Terminal 'a')]
            ,Rule (NonTerminal 'X') []
            ,Rule (NonTerminal 'Y') [Left (Terminal 'b'), Right (NonTerminal 'X')]]

testRuleMap = rulesToRuleMap testRules

parseSymbol :: ReadP Symbol
parseSymbol = do
  symbol <- satisfy isAlpha
  if isUpper symbol
    then return $ Right (NonTerminal symbol)
    else return $ Left (Terminal symbol)

parseRule :: ReadP Rule
parseRule = do
 skipSpaces
 lhs <- satisfy isUpper
 skipSpaces
 string "->"
 skipSpaces
 rhs <- many parseSymbol
 skipSpaces
 return $ Rule (NonTerminal lhs) rhs

parseGrammar :: ReadP [Rule]
parseGrammar = do
  rules <- many parseRule
  return rules

rulesToRuleMap :: [Rule] -> Map.Map NonTerminal [Rule]
rulesToRuleMap = foldl f Map.empty
  where f map r@(Rule nTerm _)  = Map.insertWith (++) nTerm [r] map

getRuleMap :: IO (Map.Map NonTerminal [Rule])
getRuleMap = do
  grammarStr <- readFile fileName
  let rules   = fst . last $ readP_to_S parseGrammar grammarStr
  return $ rulesToRuleMap rules

getTerminalsFromRule :: Rule -> [Terminal]
getTerminalsFromRule (Rule _ ss) = foldl f [] ss
  where f acc (Left terminal) = terminal:acc
        f acc (Right _)       = acc

getNonTerminalsFromRule :: Rule -> [NonTerminal]
getNonTerminalsFromRule (Rule _ ss) = foldl f [] ss
  where f acc (Left _)      = acc
        f acc (Right nTerm) = nTerm:acc

getAllTerminals :: Map.Map NonTerminal [Rule] -> [Terminal]
getAllTerminals ruleMap = foldMap f ruleMap
  where f rules = concat $ map getTerminalsFromRule rules

getAllNonTerminals :: Map.Map NonTerminal [Rule] -> [NonTerminal]
getAllNonTerminals ruleMap = foldMap f ruleMap
  where f rules = concat $ map getNonTerminalsFromRule rules

initFirstNonTerminals :: Map.Map NonTerminal [Rule] -> Map.Map Symbol FirstSet
initFirstNonTerminals ruleMap = Map.fromList $ map f nonTerminals
  where f nTerm    = (Right nTerm, Set.empty)
        nonTerminals = getAllNonTerminals ruleMap

initFirstTerminals :: Map.Map NonTerminal [Rule] -> Map.Map Symbol FirstSet
initFirstTerminals ruleMap = Map.fromList $ map f terminals
  where f term    = (Left term, Set.singleton term)
        terminals = getAllTerminals ruleMap

initFirst :: Map.Map NonTerminal [Rule] -> Map.Map Symbol FirstSet
initFirst ruleMap = initFirstNonTerminals ruleMap `Map.union` initFirstTerminals ruleMap

derivesEmptyStr :: Rule -> Map.Map NonTerminal [Rule] -> Bool
derivesEmptyStr (Rule _ []) _ = True
derivesEmptyStr (Rule _ ss) ruleMap = and $ map f ss
  where f (Left _) = False
        f (Right nTerm) =
          case Map.lookup nTerm ruleMap of
            Just rs -> or $ map (\r -> derivesEmptyStr r ruleMap) rs
            Nothing  -> error "Invalid grammar"

emptyRule :: Rule -> Bool
emptyRule (Rule _ []) = True
emptyRule _           = False

hasEmptyRule :: NonTerminal -> Map.Map NonTerminal [Rule] -> Bool
hasEmptyRule nTerm ruleMap =
  case Map.lookup nTerm ruleMap of
    Just rs -> or $ map emptyRule rs
    Nothing -> error "Invalid lookup"

initNullable :: Map.Map NonTerminal [Rule] -> Nullable
initNullable ruleMap = foldl f Set.empty (Map.keys ruleMap)
  where f nullSet nTerm = if hasEmptyRule nTerm ruleMap
                          then Set.union nullSet $ Set.singleton nTerm
                          else nullSet

getRules :: NonTerminal -> Map.Map NonTerminal [Rule] -> [Rule]
getRules nTerm ruleMap =
  case Map.lookup nTerm ruleMap of
    Nothing -> error "invalid grammar"
    Just rs -> rs

unsafeMLookup :: Ord k => k -> Map.Map k a -> a
unsafeMLookup k m =
  case Map.lookup k m of
    Nothing -> error "Unsafe lookup"
    Just x -> x

nullableSymbols :: Nullable -> [Symbol] -> Bool
nullableSymbols nullSet ss =
  case containsTerminal ss of
    True  -> False
    False -> foldl f True ss
    where f _ (Left _)        = False
          f acc (Right nTerm) = acc && Set.member nTerm nullSet
          containsTerminal (Left _ : _)    = True
          containsTerminal (Right _ : ss') = containsTerminal ss'
          containsTerminal []              = False

nullableRule :: Nullable -> Rule -> Bool
nullableRule nullSet (Rule nTerm ss)
  | Set.member nTerm nullSet = True
  | otherwise                = nullableSymbols nullSet ss

buildNullable :: Rule -> State Env ()
buildNullable r@(Rule nTerm _) = do
  env <- S.get
  if (nullableRule (nullableSet env) r)
  then put $ env { nullableSet = Set.insert nTerm (nullableSet env) }
  else return ()

buildFirstA :: NonTerminal -> [Symbol] -> Symbol -> State Env ()
buildFirstA x ss r = do
  env <- S.get
  case nullableSymbols (nullableSet env) ss of
    True -> do
      firstX <- getFirstSet (Right x)
      firstR <- getFirstSet r
      putFirstSet (Right x) $ firstX `Set.union` firstR
    False -> return ()

buildFirst:: Rule -> State Env ()
buildFirst (Rule _ []) = return ()
buildFirst r@(Rule x ss) = do
  env <- S.get
  let f [r]     = buildFirstA x [] r
      f ss      = buildFirstA x (init ss) (last ss)
  mapM_ f $ filter (\s -> length s > 0) (inits ss)

getFollowSet :: Symbol -> State Env FollowSet
getFollowSet s = do
  env <- S.get
  return $ case Map.lookup s (followMap env) of
             Just set -> set
             Nothing  -> Set.empty

putFollowSet :: Symbol -> FollowSet -> State Env ()
putFollowSet s fSet = do
  env <- S.get
  S.put $ env { followMap = Map.insert s fSet (followMap env) }

getFirstSet :: Symbol -> State Env FirstSet
getFirstSet s = do
  env <- S.get
  return $ unsafeMLookup s (firstMap env)

putFirstSet :: Symbol -> FirstSet -> State Env ()
putFirstSet s fSet =  do
  env <- S.get
  S.put $ env { firstMap = Map.insert s fSet (firstMap env) }

buildFollowA :: NonTerminal -> [Symbol] -> State Env ()
buildFollowA x (l:t) = do
  env <- S.get
  case nullableSymbols (nullableSet env) t of
    True -> do
      followL <- getFollowSet l
      followX <- getFollowSet (Right x)
      putFollowSet l $ followL `Set.union` followX
    False -> return ()

buildFollowB :: Symbol -> [Symbol] -> Symbol -> State Env ()
buildFollowB l m r = do
  env <- S.get
  case nullableSymbols (nullableSet env) m of
    True -> do
      followL <- getFollowSet l
      firstR <- getFirstSet r
      putFollowSet l $ followL `Set.union` firstR
    False -> return ()

buildFollow :: Rule -> State Env ()
buildFollow (Rule _ []) = return ()
buildFollow r@(Rule nTerm ss) = do
  let followASyms = filter (\l -> length l > 0) $ tails ss
      buildA = map (buildFollowA nTerm) followASyms
      f (l:[r]) = (l, [], r)
      f xs = (head xs, init (tail xs), last xs)
      followBSyms = map f $ filter (\l -> length l > 1) (subsequences ss)
      buildB = map (\(l,m,r) -> buildFollowB l m r) followBSyms
  sequence_ $ buildA ++ buildB

buildRule :: Rule -> State Env ()
buildRule r = do
  buildNullable r
  buildFirst r
  buildFollow r

buildNonTerminal :: NonTerminal -> State Env ()
buildNonTerminal nTerm = do
  env <- S.get
  let ruleMap = rules env
  let productions = unsafeMLookup nTerm ruleMap
  forM_ productions buildRule

buildEnv :: State Env ()
buildEnv = do
  env <- S.get
  let ruleMap = rules env
  forM_ (getAllNonTerminals ruleMap) buildNonTerminal

build :: State Env ()
build = do
  env <- S.get
  buildEnv
  env' <- S.get
  if env == env' then return () else build

initState :: Map.Map NonTerminal [Rule] -> Env
initState ruleMap = Env { rules = ruleMap
                        , nullableSet = initNullable ruleMap
                        , firstMap    = initFirst ruleMap
                        , followMap   = Map.empty }

compute :: Map.Map NonTerminal [Rule] -> State Env Env
compute ruleMap = do
  build
  S.get

result :: Map.Map NonTerminal [Rule] -> Env
result ruleMap = fst $ runState (compute ruleMap) (initState ruleMap)

main = do
  ruleMap <- getRuleMap
  let res = result ruleMap
  putStrLn $ show $ followMap res
