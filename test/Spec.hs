module Main (main) where

import Test.Tasty (defaultMain)
import qualified TreeNotation.Golden as TreeNotation

main :: IO ()
main = defaultMain =<< TreeNotation.tests
