-- https://atcoder.jp/contests/abc302/tasks/abc302_a
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Arrow hiding (arr)
import Control.Monad
import Data.List
import Data.Tuple (swap)
import Debug.Trace (trace)
import GHC.Arr

type B = Bool
type C = Char
type I = Integer
type S = String

yes :: B -> S
yes True = "Yes\n"
yes False = "No\n"

main :: IO ()
main = interact $ show . please . words
  where
    please = solve . fmap read

solve :: [I] -> I
solve [a, b] = div (a + b - 1) b
