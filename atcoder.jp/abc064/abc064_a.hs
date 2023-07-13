-- https://atcoder.jp/contests/abc064/tasks/abc064_a
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}

{-# HLINT ignore "Use infix" #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Tuple (swap)
import Debug.Trace (trace)

type B = Bool
type C = Char
type I = Integer
type S = String

-- | answer Yes or No
yes :: B -> S
yes True = "Yes\n"
yes False = "No\n"

-- | read @[Integer]@
ints :: S -> [I]
ints = fmap read . words
{-# INLINE ints #-}

-- | pairwise zipWith
pw :: (a -> a -> b) -> [a] -> [b]
pw = (<*> tail) . zipWith
{-# INLINE pw #-}

-- | divides
di :: Integral a => a -> a -> B
di = ((0 ==) .) . flip rem
{-# INLINE di #-}

main :: IO ()
main = interact $ fmap toUpper . yes . solve . ints
  where
    solve = aa

aa :: [I] -> B
aa = di 4 . foldl1 f . drop 1
  where
    f = (+) . (10 *)
