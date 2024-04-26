module Main where

-- Internal
import Test.Headers (headersPartingTest)

-- External
import Test.Tasty (defaultMain)

main :: IO ()
main = do
  defaultMain
    headersPartingTest
