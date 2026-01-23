

module ClickHaskell.Primitive.TString where

-- Internal
import ClickHaskell.Primitive.Serialization

-- GHC included
import Control.DeepSeq (NFData)
import Data.Binary.Get
import Data.ByteString as BS (ByteString, length)
import Data.ByteString.Builder
import Data.ByteString.Char8 as BS8 (pack, unpack, concatMap, singleton)
import Data.ByteString.Lazy (toStrict)
import Data.String (IsString (..))
import Prelude hiding (liftA2)



-- ** ChString

{- | ClickHouse String column type -}
newtype ChString = MkChString BS.ByteString
  deriving newtype (Show, Eq, IsString, NFData)

instance IsChType ChString where
  chTypeName = "String"
  defaultValueOfTypeName = ""

instance Serializable ChString where
  serialize rev (MkChString str) = (serialize @UVarInt rev . fromIntegral . BS.length) str <> byteString str
  deserialize rev = do
    len <- deserialize @UVarInt rev
    MkChString <$> (getByteString . fromIntegral) len
  {-# INLINE deserialize #-}

instance ToChType ChString BS.ByteString where
  toChType = MkChString
  fromChType (MkChString string) = string

instance ToChType ChString Builder where
  toChType = MkChString . toStrict . toLazyByteString
  fromChType (MkChString string) = byteString string

instance ToChType ChString String where
  toChType = MkChString . BS8.pack
  fromChType (MkChString bs)= BS8.unpack bs

instance ToQueryPart ChString where
  toQueryPart (MkChString string) =  "'" <> escapeQuery string <> "'"
    where
    escapeQuery :: BS.ByteString -> Builder
    escapeQuery = byteString . BS8.concatMap (\sym ->
      case sym of
        '\'' -> "\\\'"
        '\\' -> "\\\\"
        _ -> BS8.singleton sym
      )
