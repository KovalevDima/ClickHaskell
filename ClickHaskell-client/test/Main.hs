module Main where

import Test.Headers (headersPartingTest)

import Test.Tasty (defaultMain)

main :: IO ()
main = do
  defaultMain headersPartingTest