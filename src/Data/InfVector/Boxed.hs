module Data.InfVector.Boxed
    ( InfVector
    ) where

import           Data.InfVector
import qualified Data.Vector.Mutable as VM

type InfVector a = Infinizer (VM.IOVector a)
