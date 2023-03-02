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


-- Using derivivng mechanism, automatically generating instance of JSON class

rec :: Record
rec = Record{x = 1, y = 2, z = 3.5, s = "hello"}

testDeriveJSON :: (JSON a, Eq a) => a -> Bool
testDeriveJSON x1 =
    Ok x1 == (readJSON . showJSON) x1

tests :: Bool
tests =
    and
        [ testDeriveJSON rec,
          testDeriveJSON foo
        ]

main :: IO ()
main =
    if tests
        then return ()
        else error "Generic test failed"
