-- https://atcoder.jp/contests/abc302/tasks/abc302_b
-- https://atcoder.jp/contests/abc301/tasks/abc301_c
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
main = interact $ unlines . fmap show2 . please . words
  where
    please (h : w : xs) = solve (read h) (read w) xs
    show2 (x, y) = unwords $ show <$> [x, y]

-- show2 (x, y) = show (y + 1) ++ " " ++ show (x + 1)

d8 :: [(Int, Int) -> (Int, Int)]
d8 = add <$> ds
  where
    add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    ds = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

type A = Array (Int, Int) Char
solve :: Int -> Int -> [[Char]] -> [(Int, Int)]
solve h w cs = head aa5
  where
    -- is = index2d w h cs
    bs = toArray h w cs
    as = indices bs
    a1 = filter fs as
    a5 = filter fi [take 5 $ iterate d p1 | p1 <- a1, d <- d8]
    aa5 = filter fn a5
    fs = (== 's') . (bs !)
    fi = inRange (bounds bs) . last
    fn = (== "snuke") . fmap (bs !)

-- | 2d list to array indexd by (x, y)
toArray :: Int -> Int -> [[a]] -> Array (Int, Int) a
toArray h w = listArray ((1, 1), (h, w)) . join
