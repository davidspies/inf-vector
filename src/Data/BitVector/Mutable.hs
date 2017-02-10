{-# LANGUAGE TypeFamilies #-}

module Data.BitVector.Mutable
    ( IOBitVector
    ) where

import           Data.Bits
import           Data.InfVector.Backing
import           Data.Int
import qualified Data.Vector.Unboxed.Mutable as VUM

newtype IOBitVector = IOBitVector (VUM.IOVector Int64)

instance InfBacking IOBitVector where
  type Elem IOBitVector = Bool

  new n = fmap IOBitVector (VUM.new ((n + 63) `quot` 64))
  growAtLeast (IOBitVector v) n =
    IOBitVector <$> VUM.grow v ((n + 63) `quot` 64)
  write (IOBitVector v) i b = do
    let (q, r) = i `quotRem` 64
        br = bit r
    VUM.modify v (if b then (.|. br) else (.&. complement br)) q
  read (IOBitVector v) i = do
    let (q, r) = i `quotRem` 64
    (`testBit` r) <$> VUM.read v q
  length (IOBitVector v) = VUM.length v * 64
