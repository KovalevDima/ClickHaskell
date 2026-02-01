module ClickHaskell.Protocol.Data where

-- Internal
import ClickHaskell.Primitive

-- GHC
import Control.Exception (Exception)
import Control.Monad (forM, when)
import Data.Binary (Get)
import Data.Bits ((.&.))
import Data.ByteString (isPrefixOf)
import Data.ByteString.Builder (Builder, byteString, stringUtf8)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Int
import Data.Kind (Type)
import Data.List (mapAccumL)
import GHC.Generics
import GHC.TypeError
import GHC.TypeLits

-------------------------------------------------------------------------------
-- * Data packet
-------------------------------------------------------------------------------

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




-------------------------------------------------------------------------------
-- * Columns
-------------------------------------------------------------------------------

data Columns (columns :: [Type]) where
  Empty :: Columns '[]
  AddColumn
    :: KnownColumn (Column name chType)
    => Column name chType
    -> Columns columns
    -> Columns (Column name chType ': columns)


{- |
Column declaration

For example:

@
type MyColumn = Column "myColumn" ChString
@
-}
data Column (name :: Symbol) (chType :: Type) 

type family GetColumnName column :: Symbol where GetColumnName (Column name columnType) = name
type family GetColumnType column :: Type   where GetColumnType (Column name columnType) = columnType


class
  ( IsChType (GetColumnType column)
  , KnownSymbol (GetColumnName column)
  ) =>
  KnownColumn column where
  renderColumnName :: Builder
  renderColumnName = (stringUtf8 . symbolVal @(GetColumnName column)) Proxy

  renderColumnType :: Builder
  renderColumnType = byteString . BS8.pack $ chTypeName @(GetColumnType column)

data ColumnHeader = MkColumnHeader
  { name :: ChString
  , type_ :: ChString
  , is_custom :: UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION
  } deriving (Generic, Serializable)

mkHeader :: forall column . KnownColumn column => ColumnHeader
mkHeader = let
    name = toChType $ renderColumnName @column
    type_ = toChType $ chTypeName @(GetColumnType column)
    is_custom = AfterRevision 0
    in MkColumnHeader{..}

type ErrorHandler = UserError -> Get ()

validateColumnHeader :: forall column . KnownColumn column => ErrorHandler -> ProtocolRevision -> ColumnHeader -> Get ()
validateColumnHeader errHandler rev MkColumnHeader{..} = do
  let expectedColumnName = toChType (renderColumnName @column)
      resultColumnName = name
  when (resultColumnName /= expectedColumnName) $
    errHandler . UnmatchedColumn
      $ "Got column \"" <> show resultColumnName <> "\" but expected \"" <> show expectedColumnName <> "\""

  let expectedType = fallbackTypeName rev $ toChType (renderColumnType @column)
      resultType = fallbackTypeName rev type_
  when (resultType /= expectedType) $
    errHandler . UnmatchedType
      $ "Column " <> show resultColumnName <> " has type " <> show resultType <> ". But expected type is " <> show expectedType

fallbackTypeName :: ProtocolRevision -> ChString -> ChString
fallbackTypeName rev typeName = toChType @ChString $
  if rev < mkRev @DBMS_MIN_REVISION_WITH_TIME_ZONE_PARAMETER_IN_DATETIME_DATA_TYPE
    && isPrefixOf "DateTime(" (fromChType typeName)
  then "DateTime"
  else typeName

{- |
  Errors intended to be handled by developers
-}
data UserError
  = UnmatchedType String
  -- ^ Column type mismatch in data packet
  | UnmatchedColumn String
  -- ^ Column name mismatch in data packet
  | UnmatchedColumnsCount String
  -- ^ Occurs when actual columns count less or more than expected
  deriving (Show, Exception)




-------------------------------------------------------------------------------
-- * Columns serialization
-------------------------------------------------------------------------------

type ErrHandler = UserError -> Get ()

ignoreErr :: ErrHandler
ignoreErr _ = pure ()

class SerializableColumn col where
  {-# INLINE deserializeColumn #-}
  deserializeColumn :: KnownColumn col => ErrHandler -> ProtocolRevision -> UVarInt -> (GetColumnType col -> res) -> Get [res]
  deserializeColumn errHandler rev size f = do
    validateColumnHeader @col errHandler rev =<< deserialize @ColumnHeader rev
    deserializeColumnI @col rev size f
  
  {-# INLINE serializeColumn #-}
  serializeColumn :: KnownColumn col => ProtocolRevision -> (a -> GetColumnType col) -> [a] -> Builder
  serializeColumn rev f values =
    serialize rev (mkHeader @col) <>
    serializeColumnI @col rev (f) values

  deserializeColumnI :: ProtocolRevision -> UVarInt -> (GetColumnType col -> a) -> Get [a]
  serializeColumnI :: ProtocolRevision -> (a -> GetColumnType col) -> [a] -> Builder

instance (IsChType chType, KnownSymbol name) => KnownColumn (Column name chType)

instance
  ( Serializable chType
  , IsChType chType
  ) =>
  SerializableColumn (Column name chType) where
  {-# INLINE deserializeColumnI #-}
  deserializeColumnI rev rows f = map f <$> replicateGet rev rows

  {-# INLINE serializeColumnI #-}
  serializeColumnI rev f column = foldMap (serialize @chType rev . f) column


instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Nullable chType))
  , Serializable chType
  , IsChType chType
  ) =>
  SerializableColumn (Column name (Nullable chType)) where
  {-# INLINE deserializeColumnI #-}
  deserializeColumnI rev rows f = do
    nulls <- replicateGet @UInt8 rev rows
    forM nulls (\nulFlag -> case nulFlag of
        0 -> f . Just <$> deserialize @chType rev
        _ -> (f Nothing <$ deserialize @chType rev)
      )

  {-# INLINE serializeColumnI #-}
  serializeColumnI rev f column
    =  foldMap (serialize @UInt8 rev . maybe 1 (const 0) . f) column
    <> foldMap (serialize @chType rev . maybe defaultValueOfTypeName id . f) column

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (LowCardinality chType))
  , Serializable chType
  , IsLowCardinalitySupported chType
  , TypeError ('Text "LowCardinality deserialization still unsupported")
  ) =>
  SerializableColumn (Column name (LowCardinality chType)) where
  {-# INLINE deserializeColumnI #-}
  deserializeColumnI rev rows f = do
    _serializationType <- (.&. 0xf) <$> deserialize @UInt64 rev
    _index_size <- deserialize @Int64 rev
    -- error $ "Trace | " <> show _serializationType <> " : " <> show _index_size
    map f . coerce
      <$> replicateGet @chType rev rows

  {-# INLINE serializeColumnI #-}
  serializeColumnI _rev column = undefined column

instance {-# OVERLAPPING #-}
  ( KnownColumn (Column name (Array chType))
  , Serializable chType
  )
  => SerializableColumn (Column name (Array chType)) where
  {-# INLINE deserializeColumnI #-}
  deserializeColumnI rev rows f = do
    offsets <- replicateGet @UInt64 rev rows
    let lengths = zipWith (-) offsets (0 : (init offsets))
    forM lengths (fmap (f . MkChArray) . replicateGet @chType rev . fromIntegral)

  {-# INLINE serializeColumnI #-}
  serializeColumnI rev f column
    =  foldMap (serialize @UInt64 rev) offsets
    <> foldMap (foldMap (serialize @chType rev) . f) column
    where
    offsets =
      snd $
        mapAccumL
          (\offset xs ->
            let nextOffset = offset + fromIntegral (length xs)
            in (nextOffset, nextOffset)
          )
          0
          (map f column)

instance {-# OVERLAPPING #-}
  ( KnownSymbol name
  , IsChType chType
  , TypeError ('Text "Nested Arrays types (column \"" :<>: 'Text name :<>: 'Text "\") are unsupported")
  )
  => SerializableColumn (Column name (Array (Array chType)))
  where
  deserializeColumnI = error "Impossible"
  serializeColumnI = error "Impossible"
