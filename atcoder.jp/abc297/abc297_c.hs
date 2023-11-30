-- https://atcoder.jp/contests/abc297/tasks/abc297_c
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Debug.Trace (trace)

type ByteString = B.ByteString

newtype Parser a = Parser (ByteString -> (a, ByteString))
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

-- | convert B.read to serialiser
fromRead :: (ByteString -> Maybe (a, ByteString)) -> Parser a
-- B.tail, assuming tokens are separated by \s
fromRead r = Parser $ \bs -> let Just a = r bs in second B.tail a

class Show a => Serde a where
    ser :: a -> ByteString
    ser = B.pack . show
    des :: Parser a
    desn :: Int -> Parser [a]
    desn n = replicateM n des
instance (Serde a, Serde b) => Serde (a, b) where
    des = liftA2 (,) des des
instance Serde Bool where
    ser True = B.pack "Yes\n"
    ser False = B.pack "No\n"
instance Serde Integer where
    des = fromRead B.readInteger
instance Serde Int where
    des = fromRead B.readInt
instance Serde String where
    ser = B.pack
    des = Parser $ B.break isSpace >>> B.unpack *** B.tail
instance {-# OVERLAPS #-} Serde [String] where
    ser = B.unlines . fmap ser
instance {-# OVERLAPS #-} Serde a => Serde [a] where
    ser = B.unwords . fmap ser
    des = Parser $ unfoldr f >>> (,B.empty)
      where
        f bs | B.null bs = Nothing
        f bs = let Parser p = des in Just $ p bs

type S = String

main :: IO ()
main = B.interact $ ser . fst . p
  where
    Parser p = fmap cc $ desn 2 >>= \(a : _) -> desn a

cc :: [S] -> [S]
cc ss = f <$> ss
  where
    f :: S -> S
    f [] = []
    f [h] = [h]
    f ('T' : 'T' : t) = 'P' : 'C' : f t
    f (h : t) = h : f t
