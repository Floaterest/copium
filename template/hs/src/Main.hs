{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}

{-# HLINT ignore "Use infix" #-}

import Control.Arrow hiding (arr)
import Data.List
import Debug.Trace

type I = Integer
type S = String
type C = Char

main :: IO ()
main = interact $ unwords . solve . words
  where
    solve :: [S] -> [S]
    solve = id
