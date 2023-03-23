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
  , TypeOperators
  , ScopedTypeVariables
  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ClickHaskell.ChTypes where

import Data.ByteString       as BS (ByteString)
import Data.ByteString.Char8 as BS8 (pack)
import Data.Int              (Int32)
import Data.Text             as Text (Text, pack)
import Data.Text.Encoding    as Text (encodeUtf8)
import Data.Time             (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Proxy            (Proxy(Proxy))
import Data.String           (IsString)
import Data.UUID             as UUID (UUID, toASCIIBytes)
import Data.WideWord         (Int128)
import GHC.TypeLits          (AppendSymbol, Symbol, KnownSymbol, symbolVal)


type family (ToChTypeName columnType) :: Symbol
newtype ChTypeName = ChTypeName Text deriving newtype (Show, Semigroup, IsString)

-- | ClickHouse type-level convertion class
class KnownSymbol (ToChTypeName chType)
  => IsChType chType where
  
  -- | Get original ClickHouse type name
  --
  -- >>> originalName $ Proxy @ChInt32
  -- "Int32"
  originalName :: Proxy chType -> ChTypeName
  originalName _ = ChTypeName . Text.pack . symbolVal $ (Proxy :: Proxy (ToChTypeName chType))

  render       ::       chType -> BS.ByteString

-- | ClickHouse type requirements typeclass
class
  IsChType chType
  =>     ToChType chType     inputType where toChType :: inputType -> chType




-- | ClickHouse Nullable(T) column type (type synonym for Maybe)
type Nullable = Maybe

type NullableTypeName chType = "Nullable(" `AppendSymbol` ToChTypeName chType `AppendSymbol` ")"
type instance ToChTypeName (Maybe chType) = NullableTypeName chType

instance (KnownSymbol (NullableTypeName chType), IsChType chType)
  =>     IsChType (Maybe chType) where render = renderNullable

renderNullable :: IsChType chType => Maybe chType -> ByteString
renderNullable Nothing           = "\\N"
renderNullable (Just val)        = render val




-- | ClickHouse UUID column type
newtype                    ChUUID = ChUUID      UUID   deriving newtype (Show)
type instance ToChTypeName ChUUID = "UUID"
instance      IsChType     ChUUID where render (ChUUID uuid)   = UUID.toASCIIBytes uuid




-- | ClickHouse String column type
newtype ChString                    = ChString  Text   deriving newtype (Show, IsString)
type instance ToChTypeName ChString = "String"
instance      IsChType     ChString        where render (ChString val) = Text.encodeUtf8 val
instance      ToChType     ChString String where toChType = ChString . Text.pack
instance      ToChType     ChString Text   where toChType = ChString
instance      ToChType     ChString Int    where toChType = ChString . Text.pack . show



-- | ClickHouse Int32 column type
newtype                    ChInt32 = ChInt32    Int32  deriving newtype (Show)
type instance ToChTypeName ChInt32 = "Int32"
instance      IsChType     ChInt32       where render (ChInt32 val)   = BS8.pack $ show val
instance      ToChType     ChInt32 Int32 where toChType = ChInt32



-- | ClickHouse Int64 column type
newtype                    ChInt64 = ChInt64    Int    deriving newtype (Show)
type instance ToChTypeName ChInt64 = "Int64"
instance      IsChType     ChInt64   where render (ChInt64 val)    = BS8.pack $ show val
instance Integral a
  =>          ToChType     ChInt64 a where toChType = ChInt64 . fromIntegral




-- | ClickHouse Int128 column type
newtype ChInt128                    = ChInt128   Int128 deriving newtype (Show)
type instance ToChTypeName ChInt128 = "Int128"
instance      IsChType     ChInt128   where render (ChInt128 val)   = BS8.pack $ show val
instance Integral a
  =>          ToChType     ChInt128 a where toChType = ChInt128 . fromIntegral



-- | ClickHouse DateTime column type
newtype                    ChDateTime =  ChDateTime Int32  deriving newtype (Show)
type instance ToChTypeName ChDateTime = "DateTime"
instance      IsChType     ChDateTime where render (ChDateTime int32) = BS8.pack $ show int32




instance ToChType ChDateTime UTCTime   where toChType = ChDateTime . floor . utcTimeToPOSIXSeconds
instance ToChType ChUUID     UUID      where toChType = ChUUID
instance (KnownSymbol (NullableTypeName chType), IsChType chType, ToChType chType inputType)
  =>     ToChType (Maybe chType) (Maybe inputType) where
    toChType Nothing  = Nothing
    toChType (Just a) = Just (toChType @chType a)
