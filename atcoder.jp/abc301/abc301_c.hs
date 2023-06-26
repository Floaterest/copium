-- https://atcoder.jp/contests/abc301/tasks/abc301_c
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unqualified-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Arrow hiding (arr)
import Data.List
import Debug.Trace
import GHC.Arr

type A = Array C I
type B = Bool
type C = Char
type I = Integer
type S = String

main :: IO ()
main = interact $ yes . solve . words

yes :: B -> S
yes True = "Yes\n"
yes False = "No\n"

solve :: [S] -> B
solve st = cond1 && cond2 && cond3
  where
    [c1, c2] = count <$> st
    -- same count for chars other that atcoder
    cond1 = and $ hgf ['a' .. 'z']
    f = filter (`notElem` atc)
    g = (c1 !) &&& (c2 !)
    gf = fmap g . f
    h = uncurry (==)
    hgf = fmap h . gf
    -- has enough @s to fill
    p = max 0 . uncurry (-) . g
    r = sum . fmap p
    s = (c2 !) &&& (c1 !)
    t = sum . fmap (max 0 . uncurry (-) . s)
    cond2 = c2 ! at >= r atc
    cond3 = c1 ! at >= t atc

atc :: S
atc = "atcoder"

at :: Char
at = pred 'a'

count :: S -> A
count = accumArray (+) 0 (at, 'z') . fmap f
  where
    f '@' = (at, 1)
    f c = (c, 1)
