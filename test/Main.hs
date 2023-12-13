{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --tree-display #-}

{-
module Main (main) where

import Piob.Grammar.GrammarTest qualified
import Test.Tasty qualified as Tasty

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Piob"
    [ Piob.Grammar.GrammarTest.tests
    ]

main :: IO ()
main = Tasty.defaultMain tests
-}
