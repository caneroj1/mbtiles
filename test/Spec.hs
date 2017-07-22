module Main where

import           Control.Monad
import           Test.HUnit.Base
import           Test.HUnit.Text
import           Validation

main :: IO ()
main = void $ runTestTT $ TestList [
    TestCase validateFileDetection
  , TestCase validateSchema
  , TestCase validateMetadata
  , TestCase validateTiles
  ]
