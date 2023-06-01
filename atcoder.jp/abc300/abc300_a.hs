-- https://atcoder.jp/contests/abc300/tasks/abc300_a
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List

main :: IO ()
main = interact $ solve . lines

solve :: [String] -> String
solve s = show $ maybe 0 (+1) $ elemIndex sum arr
  where
    (_ : a : b : arr) = map read (s >>= words) :: [Int]
    sum = a + b
