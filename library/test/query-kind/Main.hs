module Main (main) where

import System.Timeout (timeout)

import ClickHaskell.Primitive (Serializable(..))
import ClickHaskell.Protocol (QueryKind(..))
import Control.Monad (forM_, unless)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  result <- timeout 5000000 runTests
  case result of
    Just () -> pure ()
    Nothing -> fail "regression suite timed out"

runTests :: IO ()
runTests = do
  -- Fixed protocol values, rather than a serializer/deserializer round trip.
  forM_ [(NoQuery, 0), (InitialQuery, 1), (SecondaryQuery, 2)] $ \(kind, code) -> do
    unless (toLazyByteString (serialize 1 kind) == BL.singleton code) $
      fail ("wrong wire code for " ++ kindName kind)
    case runGetOrFail (deserialize @QueryKind 1) (BL.singleton code) of
      Right (rest, _, decoded) | BL.null rest && kindName decoded == kindName kind -> pure ()
      _ -> fail ("wrong decoding for wire code " ++ show code)
  forM_ [3, 255] $ \code ->
    case runGetOrFail (deserialize @QueryKind 1) (BL.singleton code) of
      Left _ -> pure ()
      Right _ -> fail ("accepted invalid query kind " ++ show code)
  putStrLn "QueryKind wire-code regressions passed"

kindName :: QueryKind -> String
kindName NoQuery = "NoQuery"
kindName InitialQuery = "InitialQuery"
kindName SecondaryQuery = "SecondaryQuery"
