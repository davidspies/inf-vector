module Data.InfVector.BitVector
    ( InfBitVector
    ) where

import qualified Data.BitVector.Mutable as BVM
import           Data.InfVector

type InfBitVector = Infinizer BVM.IOBitVector
