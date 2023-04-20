{-# LANGUAGE
    DataKinds
  , DefaultSignatures
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , ScopedTypeVariables
  , UndecidableInstances
  #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module ClickHaskell.ChTypes where

import Data.ByteString       as BS (ByteString)
import Data.ByteString.Char8 as BS8 (pack, concatMap, singleton, readInt, readInteger, unpack)
import Data.Int              (Int32)
import Data.Maybe            (fromJust)
import Data.Text             as Text (Text, pack)
import Data.Text.Encoding    as Text (encodeUtf8)
import Data.Time             (UTCTime, nominalDiffTimeToSeconds, defaultTimeLocale, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Proxy            (Proxy(Proxy))
import Data.String           (IsString)
import Data.UUID             as UUID (UUID, toASCIIBytes, fromASCIIBytes)
import Data.WideWord         (Int128)
import Data.Word             (Word32)
import GHC.TypeLits          (AppendSymbol, Symbol, KnownSymbol, symbolVal, TypeError, ErrorMessage (..))
import Data.Kind (Type)
import Conduit (Identity)


type family (ToChTypeName columnType) :: Symbol
newtype ChTypeName = ChTypeName Text deriving newtype (Show, Semigroup, IsString)

-- | ClickHouse type-level convertion class
class KnownSymbol (ToChTypeName chType)
  => IsChType chType where

  -- | Get original ClickHouse type name
  --
  -- >>> originalName $ Proxy @ChInt32
  -- "Int32"
  originalName :: Proxy chType -> Text
  originalName _ = Text.pack . symbolVal $ (Proxy :: Proxy (ToChTypeName chType))

  render       :: chType -> BS.ByteString
  parse        :: BS.ByteString -> chType

-- | ClickHouse type requirements typeclass
class
  IsChType chType
  =>     ToChType chType     inputType where toChType :: inputType -> chType




-- | ClickHouse Nullable(T) column type (type synonym for Maybe)
type Nullable = Maybe

type NullableTypeName chType = "Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
type instance ToChTypeName (Maybe chType) = NullableTypeName chType

instance (KnownSymbol (NullableTypeName chType), IsChType chType)
  =>     IsChType (Maybe chType)
  where
  render = renderNullable
  parse "\\N"      = Nothing
  parse someTypeBs = Just (parse someTypeBs) :: Maybe chType

renderNullable :: IsChType chType => Maybe chType -> ByteString
renderNullable Nothing           = "\\N"
renderNullable (Just val)        = render val

instance (KnownSymbol (NullableTypeName chType), IsChType chType, ToChType chType inputType)
  =>     ToChType (Maybe chType) (Maybe inputType) where
    toChType Nothing  = Nothing
    toChType (Just a) = Just (toChType @chType a)


-- | ClickHouse LowCardinality(T) column type
type PermittedType :: Type -> Type
type family PermittedType a where
  PermittedType ChString = ChString
  PermittedType ChInt32 = ChInt32
  PermittedType ChInt64 = ChInt64
  PermittedType ChInt128 = ChInt128
  PermittedType ChDateTime = ChDateTime
  PermittedType chType = TypeError 
    ('Text "wrong type " 
    :<>: 'Text (ToChTypeName chType) 
    :<>: 'Text " couldn't be applied to LowCardinality, make sure that it uses one of next types"
    :<>: 'Text "\n ChString"
    :<>: 'Text "\n ChInt32"
    :<>: 'Text "\n ChInt64"
    :<>: 'Text "\n ChInt128"
    :<>: 'Text "\n ChDateTime")

newtype LowCardinality chType = LowCardinality (PermittedType chType)

type instance ToChTypeName (LowCardinality chType) =
  "LowCardinality(" `AppendSymbol` ToChTypeName (PermittedType chType) `AppendSymbol` ")"

instance 
  ( IsChType (LowCardinality chType)
  , ToChType (PermittedType chType) (Identity inputType)) => 
  ToChType (LowCardinality chType) (Identity inputType) where
  toChType value = LowCardinality $ toChType value

instance 
  ( KnownSymbol (ToChTypeName (LowCardinality chType))
  , IsChType chType
  , IsChType (PermittedType chType)) =>
  IsChType (LowCardinality chType) where
  render (LowCardinality value) = render value
  parse value = LowCardinality $ parse value


-- | ClickHouse UUID column type
newtype                    ChUUID = ChUUID      UUID   deriving newtype (Show)
type instance ToChTypeName ChUUID = "UUID"
instance      IsChType     ChUUID      where
  render (ChUUID uuid)   = UUID.toASCIIBytes uuid
  parse bs = ChUUID $ fromJust $ UUID.fromASCIIBytes bs
instance      ToChType     ChUUID UUID where toChType = ChUUID


-- | ClickHouse String column type
newtype ChString                    = ChString  ByteString   deriving newtype (Show, IsString)
type instance ToChTypeName ChString = "String"
instance      IsChType     ChString        where
  render (ChString val) = val
  parse = ChString
instance      ToChType     ChString String where toChType = ChString . escape . BS8.pack
instance      ToChType     ChString Text   where toChType = ChString . escape . Text.encodeUtf8
instance      ToChType     ChString Int    where toChType = ChString . escape . BS8.pack . show

escape :: ByteString -> ByteString
escape = BS8.concatMap (\sym -> if sym == '\t' then "\\t" else if sym == '\n' then "\\n" else BS8.singleton sym)


-- | ClickHouse Int32 column type
newtype                    ChInt32 = ChInt32    Int32  deriving newtype (Show)
type instance ToChTypeName ChInt32 = "Int32"
instance      IsChType     ChInt32       where
  render (ChInt32 val)   = BS8.pack $ show val
  parse = ChInt32 . fromIntegral . fst . fromJust . BS8.readInt
instance      ToChType     ChInt32 Int32 where toChType = ChInt32


-- | ClickHouse Int64 column type
newtype                    ChInt64 = ChInt64    Int    deriving newtype (Show)
type instance ToChTypeName ChInt64 = "Int64"
instance      IsChType     ChInt64   where
  render (ChInt64 val)    = BS8.pack $ show val
  parse                   = ChInt64 . fst . fromJust . BS8.readInt
instance Integral a
  =>          ToChType     ChInt64 a where toChType = ChInt64 . fromIntegral


-- | ClickHouse Int128 column type
newtype ChInt128                    = ChInt128   Int128 deriving newtype (Show)
type instance ToChTypeName ChInt128 = "Int128"
instance      IsChType     ChInt128   where
  render (ChInt128 val)   = BS8.pack $ show val
  parse                   = ChInt128 . fromInteger . fst . fromJust . BS8.readInteger
instance Integral a
  =>          ToChType     ChInt128 a where toChType = ChInt128 . fromIntegral


-- | ClickHouse DateTime column type
newtype                    ChDateTime =  ChDateTime Word32  deriving newtype (Show)
type instance ToChTypeName ChDateTime = "DateTime"
instance      IsChType     ChDateTime         where
  render (ChDateTime w32) = BS8.pack $ show w32
  parse = ChDateTime . fromInteger . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds . fromJust . parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S" . BS8.unpack
instance      ToChType     ChDateTime UTCTime where toChType = ChDateTime . floor . utcTimeToPOSIXSeconds
