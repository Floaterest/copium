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
{-# INLINE ints #-}

-- | pairwise zipWith
pairWith :: (a -> a -> b) -> [a] -> [b]
pairWith = (<*> tail) . zipWith
{-# INLINE pairWith #-}

divides :: Integral a => a -> a -> B
divides = ((0 ==) .) . flip rem
{-# INLINE divides #-}

main :: IO ()
main = interact $ unwords . solve . words
  where
    solve :: [S] -> [S]
    solve = id
