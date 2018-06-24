module Memory (Memory (Memory, hp, memory, heapsize, stacksize),
	       initialize,
	       mfetch, mstore, mstoren, mreserve)
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (foldM)
import Data.Array.IO (IOUArray, newArray_, readArray, writeArray)

data Memory = Memory { hp        :: IORef Int,
	               memory    :: IOUArray Int Int,
		       heapsize  :: Int,
		       stacksize :: Int }

initialize :: Int -> Int -> IO Memory
initialize heapsize' stacksize' =
    do p <- newIORef 4
       m <- newArray_ (0, heapsize' + stacksize')
       return $ Memory { hp = p,
			 memory = m,
			 heapsize = heapsize',
			 stacksize = stacksize' }

mfetch :: Memory -> Int -> IO Int
mfetch m a =
    readArray (memory m) (a `div` 4)

mstore :: Memory -> Int -> Int -> IO ()
mstore m p v =
    writeArray (memory m) (p `div` 4) v

mstoren :: Memory -> Int -> [Int] -> IO ()
mstoren m p vs =
    do let mem = memory m
       foldM (\ i v -> do writeArray mem i v ; return (i + 1)) (p `div` 4) vs
       return ()

mreserve :: Memory -> Int -> IO Int
mreserve m n =
    do p <- readIORef (hp m)
       writeIORef (hp m) (p + n)
       return p
