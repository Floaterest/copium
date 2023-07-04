-- https://atcoder.jp/contests/abc060/tasks/arc073_a
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}

{-# HLINT ignore "Use infix" #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Tuple (swap)
import Debug.Trace (trace)

type B = Bool
type C = Char
type I = Integer
type S = String

-- | answer Yes or No
yes :: B -> S
yes True = "Yes\n"
yes False = "No\n"

-- | read @[Integer]@
ints :: S -> [I]
ints = fmap read . words

-- | S combinator: @S x y z = x z (y z)@
sc :: (a -> b -> c) -> (a -> b) -> a -> c
sc = ((app .) .) . (&&&)

-- | e.g. @pairWith f [a, b, c] = [(f a b), (f b c)]@
pairWith :: (a -> a -> b) -> [a] -> [b]
pairWith = (`sc` tail) . zipWith

main :: IO ()
main = interact $ show . solve . ints
  where
    solve (_ : t : ns) = cc t (reverse ns)

cc :: I -> [I] -> I
cc t ns = snd $ foldr f (0, 0) ns
  where
    -- f cur tu | trace ("f " ++ show (cur, tu)) False = undefined
    f cur (end, total)
        | cur < end = (cur + t, total + t - (end - cur))
        | otherwise = (cur + t, total + t)
