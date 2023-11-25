-- https://atcoder.jp/contests/abc302/tasks/abc302_a
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Prelude hiding (read)

type ByteString = BS.ByteString

--- Parser
newtype Parser a = Parser ([ByteString] -> (a, [ByteString]))

instance Functor Parser where
    fmap f (Parser p) = Parser $ first f <$> p

instance Applicative Parser where
    pure a = Parser (a,)
    liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    liftA2 f (Parser pa) (Parser pb) = Parser $ pc . second pb . pa
      where
        pc (a, (b, bs)) = (f a b, bs)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser pa >>= f = Parser $ pb . pa
      where
        pb (a, bs) = let Parser parser = f a in parser bs

-- Readable
class Readable a where
    read :: ByteString -> a

instance Readable Integer where
    read s = let Just (res, _) = BS.readInteger s in res

next :: Readable a => Parser a
next = Parser (\(b : bt) -> (read b, bt))

main :: IO ()
main = BS.interact $ tostr . fst . p . BS.words
  where
    Parser p = aa <$> next <*> next
    tostr = BS.pack . show

aa :: Integer -> Integer -> Integer
aa a b = div (a + b - 1) b
