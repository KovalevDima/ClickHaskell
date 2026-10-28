module ClickHaskell.Internal (
  Buffer(..),
  mkBuffer,
  readBuffer,
) where
import ClickHaskell.Connection
import Data.Binary.Get
import Control.Exception


readBuffer :: Buffer -> Get a -> IO a
readBuffer MkBuffer{readBuff, writeBuff} parser = runBufferReader (runGetIncremental parser)
  where
  runBufferReader :: Decoder packet -> IO packet
  runBufferReader dec = case dec of
    (Partial decoder) -> readBuff >>= runBufferReader . decoder . Just
    (Done leftover _consumed packet) -> packet <$ writeBuff leftover
    (Fail _leftover _consumed msg) -> throwIO  (InternalError $ DeserializationError msg)
