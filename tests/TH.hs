{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import TH.Examples

tests :: Bool
tests =
    and
        [
            show (($(makeNTuple 2) 1)::(Int, Int)) == "(1,1)",
            show (($(makeNTuple 5) 0)::(Int, Int, Int, Int, Int)) == "(0,0,0,0,0)"
        ]

main :: IO ()
main =
    if tests
        then return ()
        else error "Generic test failed"
