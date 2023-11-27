-- https://atcoder.jp/contests/abc064/tasks/abc064_c
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
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
    Parser pa >>= f = Parser $ (\(a, bs) -> let Parser p = f a in p bs) . pa

class Readable a where
    next :: Parser a
    fromRead :: (ByteString -> Maybe (a, ByteString)) -> Parser a
    fromRead f = Parser $ \(b : bt) -> let Just (a, _) = f b in (a, bt)

instance Readable Integer where
    next = fromRead BS.readInteger

instance Readable [Integer] where
    next = Parser $ (>>= maybeToList . fmap fst . BS.readInteger) >>> (,[])

class ToString a where
    tostr :: a -> ByteString

instance (Show a, Num a) => ToString a where
    tostr = BS.pack . show

instance {-# OVERLAPS #-} ToString a => ToString [a] where
    tostr = BS.unwords . fmap tostr

main :: IO ()
main = BS.interact $ tostr . fst . p . BS.words
  where
    Parser p = cc <$> next

cc :: [Integer] -> [Int]
cc (_ : ns) = [max 1 a, a + b]
  where
    as = (`div` 400) <$> ns
    count f = length . filter f
    b = count (>= 8) as
    a = (length . group . sort . filter (< 8)) as
