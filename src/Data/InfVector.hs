{-# LANGUAGE NamedFieldPuns #-}

module Data.InfVector
    ( Infinizer
    , (!)
    , build
    , drop
    , head
    , iterate
    , tail
    , unfoldr
    ) where

import           Data.InfVector.Backing
import qualified Data.InfVector.Base    as BV
import           Prelude                hiding (drop, head, iterate, tail)

data Infinizer v = Infinizer
  { startInd :: Int
  , backing  :: BV.BaseVector v
  }

drop :: Int -> Infinizer v -> Infinizer v
drop n iv@Infinizer{startInd} = iv{startInd = startInd + max n 0}

(!) :: InfBacking v => Infinizer v -> Int -> Elem v
(!) Infinizer{startInd, backing} i
  | i < 0 = error "Negative index"
  | otherwise = backing BV.! (i + startInd)

unfoldr
  :: InfBacking v
  => (b -> (Elem v, b)) -> b -> Infinizer v
unfoldr iterfunc start =
  Infinizer{startInd = 0, backing = BV.unfoldr iterfunc start}

head :: InfBacking v => Infinizer v -> Elem v
head = (! 0)

tail :: Infinizer v -> Infinizer v
tail = drop 1

iterate :: InfBacking v => (Elem v -> Elem v) -> Elem v -> Infinizer v
iterate iterfunc = unfoldr (\x -> (x, iterfunc x))

build :: InfBacking v => (Int -> Elem v) -> Infinizer v
build op = unfoldr (\ind -> (op ind, ind + 1)) 0
