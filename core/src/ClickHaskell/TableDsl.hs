{-# LANGUAGE
  
  DataKinds
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
{-# LANGUAGE AllowAmbiguousTypes #-}

module ClickHaskell.TableDsl where

-- GHC boot packages
import Data.ByteString         as BS (ByteString)
import Data.Data               (Proxy(Proxy))
import Data.Kind               (Type)
import Data.Text               as T (Text, pack, unpack, intercalate)
import Data.Singletons         (demote, SingI)
import GHC.Exts                (Symbol)
import GHC.Generics            (Generic(Rep, from), Selector(selName), (:*:)(..), D1, C1, S1, M1(..), K1 (unK1))
import GHC.TypeLits            (symbolVal, KnownSymbol, TypeError, ErrorMessage (..))
import GHC.TypeLits.Singletons ()

-- Internal packages
import ClickHaskell.ChTypes (ChTypeName, ToChTypeName, ChDateTime, ChInt64, ChUUID, IsChType (render), originalName, ChString)


type ColumnRep a = Symbol

type family SupportedAndVerifiedColumn (columns :: [Type]) :: [(Symbol, Symbol)] where
  SupportedAndVerifiedColumn (x ': xs) = SupportedColumn x ': SupportedAndVerifiedColumn xs
  SupportedAndVerifiedColumn '[] = '[]

type family SupportedColumn x :: (Symbol, Symbol)
type instance SupportedColumn (DefaultColumn a b) = '(a, ToChTypeName b)


data Table (name :: Symbol) (columns :: [column :: Type]) engine
  where Table :: IsChEngine engine => Table name columns engine




showCreateTable :: (KnownSymbol name, IsChEngine engine) => String -> Proxy (Table name columns engine) -> String
showCreateTable db (_ :: Proxy (Table name columns engine)) =
  let columns = "(" <> T.intercalate ", " (map (\(first, second) -> first <> " " <> second) (getColumnsDesc @Example)) <> ")" in
  "CREATE TABLE " <> db <> "." <> symbolVal (Proxy @name) <>
  " " <> T.unpack columns  <>
  " Engine=" <> engineName (Proxy @engine)


class    IsChEngine engine    where engineName :: Proxy engine -> String
instance IsChEngine MergeTree where engineName _ = "MergeTree"
instance IsChEngine TinyLog   where engineName _ = "TinyLog"
instance {-# OVERLAPPABLE #-} TypeError
  (     'Text "Unknown table engine " ':<>: 'ShowType a
  ':$$: 'Text "Use one of the following:"
  ':$$: 'Text "  MergeTree"
  ':$$: 'Text "  TinyLog"
  ':$$: 'Text "or implement your own support"
  )  => IsChEngine a where engineName = error "Unsupported engine"
data TinyLog
data MergeTree


data DefaultColumn (name :: Symbol) columnType


type Example =
  Table
    "example"
    '[ DefaultColumn "channel_name" ChString
     , DefaultColumn "clientId"     ChInt64
     , DefaultColumn "someField"    ChDateTime
     , DefaultColumn "someField2"   ChUUID
     ]
    MergeTree




example :: [(Text, Text)]
example = getColumnsDesc @Example


getColumnsDesc :: forall t columns engine name . (IsChEngine engine, t ~ Table name columns engine, SingI (SupportedAndVerifiedColumn columns)) => [(Text, Text)]
getColumnsDesc = demote @(SupportedAndVerifiedColumn columns)






newtype ChSchema   = ChSchema { schema :: [(Text, ChTypeName)] } deriving newtype (Show, Semigroup)


class HasChSchema a where
  default getSchema :: (Generic a, GHasChSchema (Rep a)) => Proxy a -> ChSchema
  getSchema :: Proxy a -> ChSchema
  getSchema _ = toSchema (Proxy :: Proxy (Rep a))

  default toBs :: (Generic a, GToBs (Rep a)) => a -> BS.ByteString
  toBs :: a -> BS.ByteString
  toBs = (<> "\n") . gToBs . from


class GHasChSchema (p :: Type -> Type) where
  toSchema :: Proxy p -> ChSchema

instance (GHasChSchema f) => GHasChSchema (D1 c f) where toSchema _ = toSchema (Proxy @f)
instance (GHasChSchema f) => GHasChSchema (C1 c f) where toSchema _ = toSchema (Proxy @f)

instance (GHasChSchema f, GHasChSchema f2)
  => GHasChSchema (f :*: f2)   where
    toSchema _ = toSchema (Proxy @f) <> toSchema (Proxy @f2)

instance (IsChType p, Selector s)
  => GHasChSchema (S1 s (f p)) where
    toSchema _ = ChSchema [(T.pack $ selName (undefined :: t s f1 a), originalName (Proxy @p))]


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
