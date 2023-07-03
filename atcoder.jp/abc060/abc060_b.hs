-- https://atcoder.jp/contests/abc060/tasks/abc060_b
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

-- | S combinator: @S x y z = x z (y z)@
sc :: (a -> b -> c) -> (a -> b) -> a -> c
sc = ((app .) .) . (&&&)

-- | e.g. @pairWith f [a, b, c] = [(f a b), (f b c)]@
pairWith :: (a -> a -> b) -> [a] -> [b]
pairWith = (`sc` tail) . zipWith

up :: S -> S
up = fmap toUpper

ints :: S -> [I]
ints = fmap read . words

main :: IO ()
main = interact $ up . yes . solve . ints
  where
    solve [a, b, c] = bb (c, (a, b))

-- find n such that
-- na === c mod b
-- i.e. na = mb + c
-- i.e. na + mb = c
-- thus https://en.wikipedia.org/wiki/Diophantine_equation

bb :: (I, (I, I)) -> B
bb = (== 0) . app . (rem *** uncurry gcd)
