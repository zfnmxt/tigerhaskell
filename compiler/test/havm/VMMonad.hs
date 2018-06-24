module VMMonad (Mnd,
	        run, lift,
	        rfetch, rstore, rpush, rpop,
	        mfetch, mstore, mstoren, mreserve,
	        cload, cfind, cstore, cfetch,
	        lstore, lfetch,
	        optset, opttell,
                setProfileHandle, getProfileHandle,
                setDisplayHandle, getDisplayHandle,
                setTraceHandle, getTraceHandle,
	        profinc, profresult)
where

import Control.Applicative (Applicative, pure, (<*>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map, empty, fromList, toList, insert, findWithDefault)
import System.IO (Handle)

import Ir
import Cpu (Registry)
import Code (Code)
import Level (Level)
import Memory (Memory)
import Annotation (Ann)
import Opt (OptVal)
import Control.Monad (ap, liftM)
import qualified Cpu (initialize, rfetch, rstore, rpush, rpop)
import qualified Code (initialize, cload, cfind, cstore, cfetch)
import qualified Level (initialize, lfetch, lstore)
import qualified Memory (initialize, mfetch, mstore, mstoren, mreserve)

data State =
    State { reg :: Registry,
	    mem :: Memory,
	    code :: Code,
	    level :: Level,
	    option :: Map String OptVal,
            displayHandle :: Maybe Handle,
            traceHandle :: Maybe Handle,
            profileHandle :: Maybe Handle,
	    profile :: ! (Map String Int) }

newtype Mnd a =
    Mnd (IORef State -> IO a)

instance Functor Mnd where
    fmap = liftM

instance Applicative Mnd where
    pure x =
	Mnd $ \ _ ->
	    return x

    (<*>) = ap

instance Monad Mnd where
    return = pure

    (Mnd c) >>= f =
        Mnd $ \ r ->
            do x <- c r
               case f x of (Mnd c') -> c' r

run :: Mnd a -> IO a
run (Mnd c) =
    do m <- Memory.initialize 65536 16384
       r <- newIORef (s m)
       c r
    where s m = State { mem = m,
			reg = Cpu.initialize 65536 16384,
			code = Code.initialize,
			level = Level.initialize,
		        option = empty,
                        displayHandle = Nothing,
                        traceHandle = Nothing,
                        profileHandle = Nothing,
		        profile = fromList [("temp"      , 0),
					    ("binop"     , 0),
					    ("mem"       , 0),
					    ("call"      , 0),
					    ("move(temp)", 0),
					    ("move(mem)" , 0),
					    ("jump"      , 0),
					    ("cjump"     , 0)] }

lift :: IO a -> Mnd a
lift x =
    Mnd $ \ _ ->
	do r <- x
	   return r

rfetch :: String -> Mnd Int
rfetch k =
    Mnd $ \ r ->
	do s <- readIORef r
	   return $ Cpu.rfetch (reg s) k

rstore :: String -> Int -> Mnd ()
rstore k i =
    Mnd $ \ r ->
	do s <- readIORef r
	   writeIORef r $ s { reg = Cpu.rstore (reg s) k i }

rpush :: Mnd ()
rpush =
    Mnd $ \ r ->
	do s <- readIORef r
	   writeIORef r $ s { reg = Cpu.rpush (reg s) }

rpop :: Mnd ()
rpop =
    Mnd $ \ r ->
	do s <- readIORef r
	   writeIORef r $ s { reg = Cpu.rpop (reg s) }

mfetch :: Int -> Mnd Int
mfetch a =
    Mnd $ \ r ->
	do s <- readIORef r
	   Memory.mfetch (mem s) a

mstore :: Int -> Int -> Mnd ()
mstore a i =
    Mnd $ \ r ->
	do s <- readIORef r
	   Memory.mstore (mem s) a i

mstoren :: Int -> [Int] -> Mnd ()
mstoren a is =
    Mnd $ \ r ->
	do s <- readIORef r
	   Memory.mstoren (mem s) a is

mreserve :: Int -> Mnd Int
mreserve n =
    Mnd $ \ r ->
	do s <- readIORef r
	   Memory.mreserve (mem s) n

cload :: String -> [Stm Ann] -> Mnd ()
cload a c =
    Mnd $ \ r ->
	do s <- readIORef r
	   writeIORef r $ s { code = Code.cload (code s) a c }

cfind :: String -> Mnd [Stm Ann]
cfind a =
    Mnd $ \ r ->
	do s <- readIORef r
	   return $ Code.cfind (code s) a

cstore :: String -> Int -> Mnd ()
cstore label pointer =
    Mnd $ \ reference ->
	do state <- readIORef reference
	   writeIORef reference $ state { code = Code.cstore (code state) label pointer }

cfetch :: String -> Mnd Int
cfetch label =
    Mnd $ \ reference ->
	do state <- readIORef reference
	   return $ Code.cfetch (code state) label

lstore :: String -> Int -> Mnd ()
lstore label depth =
    Mnd $ \ reference ->
	do state <- readIORef reference
	   writeIORef reference $ state { level = Level.lstore (level state) label depth }

lfetch :: String -> Mnd Int
lfetch label =
    Mnd $ \ reference ->
	do state <- readIORef reference
	   return $ Level.lfetch (level state) label

optset :: String -> OptVal -> Mnd ()
optset name value =
    Mnd $ \ reference ->
	do state <- readIORef reference
	   writeIORef reference $ state { option = insert name value (option state) }

opttell :: String -> OptVal -> Mnd OptVal
opttell name value =
    Mnd $ \ reference ->
	do state <- readIORef reference
	   return $ findWithDefault value name (option state) 

setProfileHandle :: Handle -> Mnd ()
setProfileHandle h =
    Mnd $ \ reference ->
        do state <- readIORef reference
           writeIORef reference $ state { profileHandle = Just h }

getProfileHandle :: Mnd (Maybe Handle)
getProfileHandle =
    Mnd $ \ reference ->
        do state <- readIORef reference
           return $ (profileHandle state)

setDisplayHandle :: Handle -> Mnd ()
setDisplayHandle h =
    Mnd $ \ reference ->
        do state <- readIORef reference
           writeIORef reference $ state { displayHandle = Just h }

getDisplayHandle :: Mnd (Maybe Handle)
getDisplayHandle =
    Mnd $ \ reference ->
        do state <- readIORef reference
           return $ (displayHandle state)

setTraceHandle :: Handle -> Mnd ()
setTraceHandle h =
    Mnd $ \ reference ->
        do state <- readIORef reference
           writeIORef reference $ state { traceHandle = Just h }

getTraceHandle :: Mnd (Maybe Handle)
getTraceHandle =
    Mnd $ \ reference ->
        do state <- readIORef reference
           return $ (traceHandle state)

profinc :: String -> Mnd ()
profinc key =
    Mnd $ \ reference ->
	do state <- readIORef reference
	   writeIORef reference $ state { profile = insert key (findWithDefault 0 key (profile state) + 1 ) (profile state) }

profresult :: Mnd (Map String Int)
profresult =
    Mnd $ \ reference ->
	do state <- readIORef reference
	   return $ profile state
