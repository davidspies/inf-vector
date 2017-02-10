{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}

module Data.InfVector.Base
    ( BaseVector
    , unfoldr
    , (!)
    ) where

import           Control.Monad
import           Control.Monad.Loops
import           Data.InfVector.Backing as InfBacking
import           Data.IORef
import           System.IO.Unsafe

data BaseVector v = forall b. BaseVector
  { iterfunc :: b -> (Elem v, b)
  , vecRef   :: IORef v
  , topRef   :: IORef (Int, b)
  }

initialCapacity :: Int
initialCapacity = 2

{-# NOINLINE unfoldr #-}
unfoldr :: InfBacking v => (b -> (Elem v, b)) -> b -> BaseVector v
unfoldr iterfunc start = BaseVector
  { iterfunc
  , vecRef = unsafePerformIO $ new initialCapacity >>= newIORef
  , topRef = unsafePerformIO $ newIORef (0, start)
  }

{-# NOINLINE (!) #-}
(!) :: InfBacking v => BaseVector v -> Int -> Elem v
(!) BaseVector{iterfunc, vecRef, topRef} i =
  unsafePerformIO $ do
    capacity <- InfBacking.length <$> readIORef vecRef
    when (capacity <= i) $ do
      let growcap = max (i + 1 - capacity) capacity
      curvec <- readIORef vecRef
      nextvec <- growAtLeast curvec growcap
      writeIORef vecRef nextvec
    v <- readIORef vecRef
    whileM_ ((<= i) . fst <$> readIORef topRef) $ do
      (curlen, curgen) <- readIORef topRef
      let (curval, nextgen) = iterfunc curgen
      write v curlen curval
      writeIORef topRef (curlen + 1, nextgen)
    InfBacking.read v i
