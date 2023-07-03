-- https://atcoder.jp/contests/abc060/tasks/abc060_b
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant bracket" #-}

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

main :: IO ()
main = interact $ fmap toUpper . yes . solve . words
  where
    solve = bb . fmap read

-- na === c mod b
-- na = mb + c
-- na + mb = c

bb :: [I] -> B
bb [a, b, c] = (rem c (gcd a b)) == 0
