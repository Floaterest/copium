-- https://atcoder.jp/contests/abc301/tasks/abc301_a
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.List

type I = Integer
type S = String
type C = Char

main :: IO ()
main = interact $ show . func . words
  where
    func [n, s] = solve ((`div` 2) . (+ 1) . read $ n) 0 0 s

solve :: I -> I -> I -> S -> C
solve n t a _
    | a >= n = 'A'
    | t >= n = 'T'
solve n t a ('A' : s) = solve n t (a + 1) s
solve n t a ('T' : s) = solve n (t + 1) a s
