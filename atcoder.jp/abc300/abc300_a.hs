-- https://atcoder.jp/contests/abc300/tasks/abc300_a
-- please work
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List

main :: IO ()
main = interact $ solve . lines

solve :: [String] -> String
solve [nab, s] = show $ i + 1
  where
    i = unwrap $ elemIndex sum arr
    [_, a, b] = map read $ words nab
    arr = map read $ words s
    sum = a + b

unwrap (Just n) = n
