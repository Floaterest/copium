-- https://atcoder.jp/contests/abc297/tasks/abc297_d
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

class Show a => Serde a where
    ser :: a -> ByteString
    ser = BS.pack . show

    re :: ByteString -> Maybe (a, ByteString)
    des :: Parser a
    des = Parser $ \(b : bt) -> let Just (a, _) = re b in (a, bt)

    desn :: Int -> Parser [a]
    desn n = replicateM n des

instance Serde Bool where
    ser True = BS.pack "Yes\n"
    ser False = BS.pack "No\n"

instance Serde Integer where
    re = BS.readInteger

instance Serde String where
    ser = BS.pack
    des = Parser $ \(b : bt) -> (BS.unpack b, bt)

instance {-# OVERLAPS #-} Serde a => Serde [a] where
    ser = BS.unwords . fmap ser
    des = Parser $ (>>= maybeToList . fmap fst . re) >>> (,[])

main :: IO ()
main = BS.interact $ ser . fst . p . BS.words
  where
    Parser p = aa <$> desn 2

aa :: [Integer] -> Integer
aa [n, m] = subtract 1 $ sum $ unfoldr f (n, m)
  where
    f (_, b) | b == 0 = Nothing
    f (a, b) = Just (a `div` b, let u = a `mod` b in (max u b, min u b))
