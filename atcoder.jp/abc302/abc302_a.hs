-- https://atcoder.jp/contests/abc302/tasks/abc302_a
{-# LANGUAGE FlexibleInstances #-}
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

type ByteString = BS.ByteString

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

class Readable a where
    next :: Parser a
    fromRead :: (ByteString -> Maybe (a, ByteString)) -> Parser a
    fromRead f = Parser pa
      where
        pa (b : bt) = let Just (res, _) = f b in (res, bt)

instance Readable Integer where
    next = fromRead BS.readInteger

instance Readable [Integer] where
    next = Parser p
      where
        p bs = (bs >>= maybeToList . fmap fst . BS.readInteger, [])

main :: IO ()
main = BS.interact $ tostr . fst . p . BS.words
  where
    Parser p = aa <$> next
    tostr = BS.pack . show

aa :: [Integer] -> Integer
aa [a, b] = div (a + b - 1) b
