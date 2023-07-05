-- https://atcoder.jp/contests/abc060/tasks/arc073_a
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

-- | pairwise zipWith
pw :: (a -> a -> b) -> [a] -> [b]
-- <*> is the S combinator :: (a -> b -> c) -> (a -> b) -> (a -> c)
-- https://en.wikipedia.org/wiki/SKI_combinator_calculus
pw = (<*> tail) . zipWith

main :: IO ()
main = interact $ show . solve . ints
  where
    solve (_ : t : ns) = cc t ns

cc :: I -> [I] -> I
cc = (.) <$> sm <*> sb
  where
    sm = (. sum) . (+)
    sb = (. pw subtract) . fmap . min
