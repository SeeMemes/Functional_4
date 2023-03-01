{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Text.SimpleJSON
import Data.Data
import Text.SimpleJSON.String ()
import Text.SimpleJSON.DeriveVia
import Text.ParserCombinators.Parsec ()


data Foo = Foo{a :: Int, b :: Bool, c :: Baz} | None
    deriving (Typeable, Data, Show, Eq, JSON)

foo = Foo{a = 0, b = True, c = (Baz 5)}

data Record = Record{x :: Int, y :: Double, z :: Float, s :: String} | EmptyDummy
    deriving (Typeable, Data, Show, Eq)
    deriving (JSON)
        via CustomJSON Record

newtype Baz = Baz Int
    deriving (Typeable, Data, Show, Eq)

data Bar = Int :+: Int | Zero
    deriving (Typeable, Data, Show, Eq)

newtype New a = New a
    deriving (Typeable, Data, Show, Eq)

newtype Apples = Apples {noApples :: Int}
    deriving (Typeable, Data, Show, Eq)


-- Using derivivng mechanism, automatically generating instance of JSON class

rec :: Record
rec = Record{x = 1, y = 2, z = 3.5, s = "hello"}

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Typeable, Data, Show, Eq)

atree :: Tree Integer
atree = build 4
    where
        build 0 = Leaf
        build 1 = Node Leaf 100 Leaf
        build n = Node (build (n - 1)) n (build (n - 2))

data Color = Red | Green | Blue
    deriving (Typeable, Data, Show, Eq, Enum)

testJSON :: (Data a, Eq a) => a -> Bool
testJSON x1 =
    x1 == decodeJSON (encodeJSON x1)

testDeriveJSON :: (JSON a, Eq a) => a -> Bool
testDeriveJSON x1 =
    Ok x1 == (readJSON . showJSON) x1

tests :: Bool
tests =
    and
        [ testJSON (1 :: Integer),
          testJSON (42 :: Int),
          testJSON (4.2 :: Double),
          testJSON (4.1 :: Float),
          testJSON (True :: Bool),
          testJSON 'q',
          testJSON "Hello, World\n",
          testJSON ([] :: [Bool]),
          testJSON ([1, 2, 3, 4] :: [Integer]),
          testJSON $ Baz 11,
          testJSON $ Foo 1 True (Baz 42),
          testJSON None,
          testJSON Zero,
          testJSON rec,
          testJSON atree,
          testJSON $ Apples 42,
          testJSON [Red .. Blue],
          testDeriveJSON rec,
          testDeriveJSON foo
        ]

main :: IO ()
main =
    if tests
        then return ()
        else error "Generic test failed"
