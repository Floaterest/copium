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
-- su for substitution
su :: (a -> b -> c) -> (a -> b) -> a -> c
su = ((app .) .) . (&&&)

-- | e.g. @pairWith f [a, b, c] = [(f a b), (f b c)]@
pairWith :: (a -> a -> b) -> [a] -> [b]
pairWith = (`su` tail) . zipWith

main :: IO ()
main = interact $ show . solve . fmap read . words
  where
    solve (_ : t : ns) = cc t ns

cc :: I -> [I] -> I
cc t ns = (+ t) . sum . (min t <$>) $ pairWith subtract ns
