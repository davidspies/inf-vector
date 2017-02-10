module Data.InfVector.Unboxed
    ( InfVector
    ) where

import           Data.InfVector
import qualified Data.Vector.Unboxed.Mutable as VUM

type InfVector a = Infinizer (VUM.IOVector a)
