-- https://atcoder.jp/contests/abc301/tasks/abc301_b
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Arrow hiding (arr)
import Data.List
import Debug.Trace

type I = Integer
type S = String
type C = Char

main :: IO ()
main = interact $ unwords . solve . words
  where
    solve (_ : cs) = show <$> f (read <$> cs)

f :: [I] -> [I]
f (a : b : ns)
    | a < b = [a .. b - 1] ++ arr
    | a > b = [a, a - 1 .. b + 1] ++ arr
  where
    arr = f (b : ns)
f [n] = [n]
