module ClickHaskell.Protocol.Data where

-- Internal
import ClickHaskell.Primitive

-- GHC
import Data.Int
import GHC.Generics

-- * Common Data packet

data DataPacket = MkDataPacket
  { table_name    :: ChString
  , block_info    :: BlockInfo
  , columns_count :: UVarInt
  , rows_count    :: UVarInt
  }
  deriving (Generic, Serializable)

mkDataPacket :: ChString -> UVarInt -> UVarInt -> DataPacket
mkDataPacket table_name columns_count rows_count =
  MkDataPacket
      { table_name
      , block_info    = MkBlockInfo
        { field_num1   = 1, is_overflows = 0
        , field_num2   = 2, bucket_num   = -1
        , eof          = 0
        }
      , columns_count
      , rows_count
      }

data BlockInfo = MkBlockInfo
  { field_num1   :: UVarInt, is_overflows :: UInt8
  , field_num2   :: UVarInt, bucket_num   :: Int32
  , eof          :: UVarInt
  }
  deriving (Generic, Serializable)
