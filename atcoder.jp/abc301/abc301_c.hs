-- https://atcoder.jp/contests/abc301/tasks/abc301_c
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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

type A = Array C I
type B = Bool
type C = Char
type I = Integer
type S = String

main :: IO ()
main = interact $ yes . please . fmap count . words
  where
    please [c1, c2] = solve (c1, c2)

solve, same, diff :: (A, A) -> B
solve = and . sequence [same, diff, diff . swap]
-- same occurences of chars except atcoder
same (c1, c2) = and (uncurry (==) . countc c1 c2 <$> filter (`notElem` a) ['a' .. 'z'])
-- check has enough @s to fill atcoder
diff (c1, c2) = (<= c2 ! at) $ sum $ fmap (max 0 . uncurry (-) . countc c1 c2) a

at :: C
at = pred 'a'

a :: S
a = "atcoder"

countc :: A -> A -> C -> (I, I)
countc c1 c2 = (c1 !) &&& (c2 !)

yes :: B -> S
yes True = "Yes\n"
yes False = "No\n"

count :: S -> A
count = accumArray (+) 0 (at, 'z') . fmap f
  where
    f '@' = (at, 1)
    f c = (c, 1)
