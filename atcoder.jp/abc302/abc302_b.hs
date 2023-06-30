-- https://atcoder.jp/contests/abc302/tasks/abc302_b
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
import Data.Maybe
import Data.Tuple (swap)
import Debug.Trace (trace)

type B = Bool
type C = Char
type I = Integer
type S = String

-- yes :: B -> S
-- yes True = "Yes\n"
-- yes False = "No\n"

main :: IO ()
main = interact $ unlines . fmap show2 . please . words
  where
    please (h : w : xs) = solve (read h) (read w) xs
    show2 (x, y) = unwords $ show <$> [x, y]

d8 :: [(Int, Int) -> (Int, Int)]
d8 = add <$> ds
  where
    add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    ds = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

type A = Array (Int, Int) Char
solve :: Int -> Int -> [[Char]] -> [(Int, Int)]
solve h w cs = fromMaybe [] a5
  where
    as = toArray h w cs
    a1 = filter ((== 's') . (as !)) (indices as)
    t5 d = take 5 . iterate d
    a5 = find (und . (fin &&& fsn)) (t5 <$> d8 <*> a1)
    fin = inRange (bounds as) . last
    fsn = (== "snuke") . fmap (as !)
    und (b1, b2) = b1 && b2 -- Deutch goes brr

-- | 2d list to array indexd by (x, y)
toArray :: Int -> Int -> [[a]] -> Array (Int, Int) a
toArray h w = listArray ((1, 1), (h, w)) . join
