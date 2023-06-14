{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}

{-# HLINT ignore "Use infix" #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Debug.Trace

type I = Integer
type S = String
type C = Char

main :: IO ()
main = interact $ unwords . solve . words
  where
    solve :: [S] -> [S]
    solve = id
