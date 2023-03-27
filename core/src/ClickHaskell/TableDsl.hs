{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DerivingStrategies
  , FlexibleInstances
  , FlexibleContexts
  , GADTs
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , PolyKinds
  , TypeFamilies
  , TypeOperators
  , TypeApplications
  , ScopedTypeVariables
  , UndecidableInstances
#-}
{-# LANGUAGE DeriveAnyClass #-}

module ClickHaskell.TableDsl where

import Data.ByteString         as BS (ByteString)
import Data.ByteString.Char8   as BS8 (takeWhile, dropWhile)
import Data.Data               (Proxy(Proxy))
import Data.Kind               (Type)
import Data.Text               as T (Text, pack, unpack, intercalate)
import Data.Singletons         (demote, SingI)
import GHC.Generics            (Generic(Rep, from, to), Selector(selName), (:*:)(..), D1, C1, S1, M1(..), K1(unK1, K1))
import GHC.TypeLits            (symbolVal, KnownSymbol, TypeError, ErrorMessage(..), Symbol)
import GHC.TypeLits.Singletons ()

import Data.UUID (nil)

import ClickHaskell.ChTypes (IsChType(render, originalName), ToChTypeName, ChInt64, ChUUID, ChString, parse, toChType, ChDateTime)


type family SupportedAndVerifiedColumns (columns :: [Type]) :: [(Symbol, Symbol)] where
  SupportedAndVerifiedColumns (x ': '[]) = SupportedColumn x ': '[]
  SupportedAndVerifiedColumns (x ': xs)  = SupportedColumn x ': SupportedAndVerifiedColumns xs
  SupportedAndVerifiedColumns '[]        = TypeError ('Text "No columns in table")

type family SupportedColumn x :: (Symbol, Symbol)
type instance SupportedColumn (DefaultColumn a b) = '(a, ToChTypeName b)

data DefaultColumn (name :: Symbol) columnType


data InDatabase
  (db :: Symbol)
  t
  where
  InDatabase :: InDatabase db t

data Table
  (name :: Symbol)
  (columns :: [column :: Type])
  engine
  (partitionBy :: [Symbol])
  (orderBy     :: [Symbol])
  where
  Table :: IsChEngine engine => Table name columns engine partitionBy orderBy


showCreateTable :: forall t db name columns engine orderBy partitionBy .
  ( t ~ InDatabase db (Table name columns engine orderBy partitionBy)
  , KnownSymbol name
  , KnownSymbol db
  , SingI partitionBy
  , SingI orderBy
  , SingI (SupportedAndVerifiedColumns columns)
  , IsChEngine engine
  ) => String
showCreateTable =
  let columns     = demote @(SupportedAndVerifiedColumns columns)
      partitionBy = demote @partitionBy
      orderBy     = demote @orderBy
  in "CREATE TABLE "  <> symbolVal (Proxy @db) <> "." <> symbolVal (Proxy @name)
  <> " "              <> T.unpack ("(" <> T.intercalate ", " (map (\(first, second) -> first <> " " <> second) columns) <> ")")
  <> " Engine="       <> engineName @engine
  <> " PARTITION BY " <> (if null partitionBy then "tuple()" else T.unpack ("(" <> T.intercalate ", " partitionBy <> ")"))
  <> " ORDER BY "     <> (if null orderBy     then "tuple()" else T.unpack ("(" <> T.intercalate ", " orderBy     <> ")"))




class    IsChEngine engine    where engineName :: String
instance IsChEngine MergeTree where engineName = "MergeTree"
instance IsChEngine TinyLog   where engineName = "TinyLog"
instance {-# OVERLAPPABLE #-} TypeError
  (     'Text "Unknown table engine " ':<>: 'ShowType a
  ':$$: 'Text "Use one of the following:"
  ':$$: 'Text "  MergeTree"
  ':$$: 'Text "  TinyLog"
  ':$$: 'Text "or implement your own support"
  )  => IsChEngine a where engineName = error "Unsupported engine"
data TinyLog
data MergeTree




type Example =
  Table
    "example"
    '[ DefaultColumn "channel_name" ChString
     , DefaultColumn "clientId"     ChInt64
     , DefaultColumn "someField"    ChDateTime
     , DefaultColumn "someField2"   ChUUID
     ]
    MergeTree
    '[]
    '[]

data ExampleData = ExampleData
  { channel_name :: ChString
  , clientId     :: ChString
  , someField    :: ChString
  , someField2   :: ChString
  }
  deriving (Generic, HasChSchema, Show)

example :: [(Text, Text)]
example = getColumnsDesc @Example

exampleData :: ExampleData
exampleData = ExampleData
  (toChType ("text\t" :: Text))
  (toChType $ show 42)
  (toChType $ show nil)
  (toChType ("hello World" :: Text))




getColumnsDesc :: forall t columns engine name orderBy partitionBy .
  ( t ~ Table name columns engine orderBy partitionBy
  , SingI (SupportedAndVerifiedColumns columns)
  )
  => [(Text, Text)]
getColumnsDesc = demote @(SupportedAndVerifiedColumns columns)


class HasChSchema a where
  default getSchema :: (Generic a, GHasChSchema (Rep a)) => Proxy a -> [(Text, Text)]
  getSchema :: Proxy a -> [(Text, Text)]
  getSchema _ = toSchema @(Rep a)

  default toBs :: (Generic a, GToBs (Rep a)) => a -> BS.ByteString
  toBs :: a -> BS.ByteString
  toBs = (<> "\n") . gToBs . from
  {-# INLINE toBs #-}

  default fromBs :: (Generic a, GFromBS (Rep a)) => BS.ByteString -> a
  fromBs :: BS.ByteString -> a
  fromBs = to . gFromBs


class GHasChSchema (p :: Type -> Type) where
  toSchema :: [(Text, Text)]

instance (GHasChSchema f) => GHasChSchema (D1 c f) where toSchema = toSchema @f
instance (GHasChSchema f) => GHasChSchema (C1 c f) where toSchema = toSchema @f

instance (GHasChSchema f, GHasChSchema f2)
  => GHasChSchema (f :*: f2)   where
    toSchema = toSchema @f <> toSchema @f2

instance (IsChType p, Selector s)
  => GHasChSchema (S1 s (f p)) where
    toSchema = [(T.pack $ selName (undefined :: t s f1 a), originalName (Proxy @p))]


class GToBs f where
  gToBs :: f p -> BS.ByteString

instance GToBs f => GToBs (D1 c f) where
  gToBs (M1 re) = gToBs re
  {-# INLINE gToBs #-}
instance GToBs f => GToBs (C1 c f) where
  gToBs (M1 re) = gToBs re
  {-# INLINE gToBs #-}

instance (GToBs f, GToBs f2)
  => GToBs (f :*: f2) where
  gToBs (f :*: f2) = gToBs f <> "\t" <> gToBs f2
  {-# INLINE gToBs #-}

instance (IsChType p)
  => GToBs (S1 s (K1 i p)) where
  gToBs (M1 re) = render $ unK1 re
  {-# INLINE gToBs #-}


class GFromBS f where
  gFromBs :: BS.ByteString -> f p

instance GFromBS f => GFromBS (D1 c f) where
  gFromBs bs = M1 $ gFromBs bs
  {-# INLINE gFromBs #-}
instance GFromBS f => GFromBS (C1 c f) where
  gFromBs bs = M1 $ gFromBs bs
  {-# INLINE gFromBs #-}

instance (GFromBS f1, GFromBS f2)
  => GFromBS (f1 :*: f2) where
  gFromBs bs =
    gFromBs (BS8.takeWhile (/= '\t') bs)
    :*:
    gFromBs (BS8.dropWhile (=='\t') . BS8.dropWhile (/= '\t') $ bs)
  {-# INLINE gFromBs #-}

instance (IsChType p)
  => GFromBS (S1 s (K1 i p)) where

  gFromBs :: IsChType p => ByteString -> S1 s (K1 i p) p1
  gFromBs bs = M1 $ K1 $ parse bs
  {-# INLINE gFromBs #-}
