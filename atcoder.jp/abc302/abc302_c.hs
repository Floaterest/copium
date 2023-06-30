-- https://atcoder.jp/contests/abc302/tasks/abc302_c
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unrecognised-warning-flags #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Tuple (swap)
import Debug.Trace (trace)

type B = Bool
type C = Char
type I = Integer
type S = String

yes :: B -> S
yes True = "Yes\n"
yes False = "No\n"

main :: IO ()
main = interact $ yes . please . words
  where
    please (_ : _ : xs) = solve xs

solve cs = or ds
  where
    ps :: [[S]]
    ps = permutations cs
    cond s t = (1 ==) $ length $ filter not $ zipWith (==) s t
    bs = [zipWith cond p (tail p) | p <- ps]
    ds = and <$> bs
