{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.InfVector.Backing
    ( InfBacking(..)
    ) where

import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed.Mutable as VUM

class InfBacking v where
  type Elem v

  new :: Int -> IO v
  growAtLeast :: v -> Int -> IO v
  write :: v -> Int -> Elem v -> IO ()
  read :: v -> Int -> IO (Elem v)
  length :: v -> Int

instance VUM.Unbox a => InfBacking (VUM.IOVector a) where
  type Elem (VUM.IOVector a) = a

  new = VUM.new
  growAtLeast = VUM.grow
  write = VUM.write
  read = VUM.read
  length = VUM.length

instance InfBacking (VM.IOVector a) where
  type Elem (VM.IOVector a) = a
  new = VM.new
  growAtLeast = VM.grow
  write = VM.write
  read = VM.read
  length = VM.length
