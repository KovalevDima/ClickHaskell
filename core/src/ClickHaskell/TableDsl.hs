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

module ClickHaskell.TableDsl where

import Data.ByteString         as BS (ByteString)
import Data.ByteString.Char8   as BS8 (split, intercalate)
import Data.Data               (Proxy(Proxy))
import Data.Kind               (Type)
import Data.Text               as T (Text, pack, unpack, intercalate)
import Data.Text.Lazy.Builder  (Builder, fromString)
import Data.Singletons         (demote, SingI)
import GHC.Generics            (Generic(Rep, from, to), Selector(selName), (:*:)(..), D1, C1, S1, M1(..), K1(unK1, K1), V1, U1)
import GHC.TypeLits            (symbolVal, KnownSymbol, TypeError, ErrorMessage(..), Symbol)
import GHC.TypeLits.Singletons ()

import ClickHaskell.ChTypes      (IsChType(originalName, parse, render), ToChTypeName)



data SampledBy (fieldName :: Symbol) (conditionalExpression :: Type) handlingData where 
  MkSampledBy :: handlingData -> SampledBy fieldName conditionalExpression handlingData
  deriving (Generic, Show, Functor)

type Unwraped :: Type -> Type
type family Unwraped t where
  Unwraped (SampledBy fieldName conditionalExpression handlingData) = Unwraped handlingData
  Unwraped handlingData = handlingData



 
class ToConditionalExpression t where
  toConditionalExpression :: Builder

instance {-# OVERLAPS #-}
  ( ToConditionalExpression handlingData
  , ToConditionalExpPart conditionalExpPart
  , KnownSymbol fieldName
  ) => 
  ToConditionalExpression (SampledBy fieldName conditionalExpPart handlingData) where
  toConditionalExpression = fromString (symbolVal (Proxy @fieldName)) <> "=" <> toConditionalExpPart @conditionalExpPart <> " AND " <> toConditionalExpression @handlingData 


instance {-# OVERLAPPING #-} HasChSchema handlingData => (ToConditionalExpression handlingData) where
  toConditionalExpression = ""


class ToConditionalExpPart a where
  toConditionalExpPart :: Builder 
  
data EqualityWith (a :: Symbol)
instance KnownSymbol a => ToConditionalExpPart (EqualityWith a) where toConditionalExpPart = "='" <> fromString (symbolVal (Proxy @a)) <> "'"
data Infixion      a



type MapFst :: [(a, b)] -> [a]
type family MapFst xs where
  MapFst ('(a, b) ': xs) = a ': MapFst xs
  MapFst '[] = '[]


type ValidatedRequest :: Type -> Type -> Symbol
type family ValidatedRequest handlingDataDescripion table where
  ValidatedRequest handlingDataDescripion table = ""


-- ToDo 1: to implement types equality and inclusion check
type ValidatedSubset :: [(Symbol, Type)] -> [(Symbol, Type)] -> [(Symbol, Type)]
type family ValidatedSubset a b where
  ValidatedSubset ('(field, fieldType) ': columns1) columns2 = columns2


-- column solver
type ColumnSolve :: (Symbol, Type) -> [(Symbol, Type)] -> (Symbol, Type)
type family ColumnSolve a as where
  ColumnSolve '(field, fieldType) ('(field, fieldType) ': fields) = 
    '(field, fieldType)
  ColumnSolve '(field, fieldType1) ('(field, fieldType2) ': fields) =
    TypeError ('Text "There is a field " ':<>: 'Text field ':<>: 'Text " had duplicated type declarations")
  ColumnSolve '(field, _) '[] =
    TypeError ('Text "There is a field " ':<>: 'Text field ':<>: 'Text " that is out of table")
  ColumnSolve column (column' ': columns) = ColumnSolve column columns


-- ToDo 2: no duplicated columns check
type family TransformedToSupportedColumns (columns :: [Type]) :: [(Symbol, Symbol)] where
  TransformedToSupportedColumns (x ': '[]) = SupportedColumn x ': '[]
  TransformedToSupportedColumns (x ': xs)  = SupportedColumn x ': TransformedToSupportedColumns xs
  TransformedToSupportedColumns '[]        = TypeError ('Text "No columns in table")


-- supported endpoint of columns
type family SupportedAndVerifiedColumns (columns :: [Type]) :: [(Symbol, Symbol)] where
  SupportedAndVerifiedColumns xs = NoDuplicated (TransformedToSupportedColumns xs)


-- no dublicated typelevel list
type NoDuplicated :: [(Symbol, Symbol)] -> [(Symbol, Symbol)]
type family NoDuplicated xs where
  NoDuplicated (x ': xs) = ElemOrNot x xs ': NoDuplicated xs
  NoDuplicated '[] = '[]


-- type level solver
type ElemOrNot :: a -> [a] -> a
type family ElemOrNot a as where
  ElemOrNot a '[] = a
  ElemOrNot '(a, _) ('(a, _) ': xs) = 
    TypeError ('Text "There is a field " ':<>: 'Text a ':<>: 'Text " duplicated")
  ElemOrNot '(a, c) ('(b, _) ': xs) = ElemOrNot '(a, c) xs


type family SupportedColumn x :: (Symbol, Symbol) where
  SupportedColumn (DefaultColumn a b) = '(a, ToChTypeName b)


data DefaultColumn (name :: Symbol) columnType


data InDatabase
  (db :: Symbol)
  (t :: Type)
  where
  InDatabase :: InDatabase db t

data Table
  (name :: Symbol)
  (columns :: [column :: Type])
  engine
  (partitionBy :: [Symbol])
  (orderBy     :: [Symbol])
  where
  Table :: forall name columns engine partitionBy orderBy . IsChEngine engine => Table name columns engine partitionBy orderBy


showCreateTable :: forall t db table name columns engine orderBy partitionBy .
  ( table ~ Table name columns engine orderBy partitionBy
  , t ~ InDatabase db table
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

instance (HasChSchema handlingData) => HasChSchema (SampledBy fieldName conditionalExpression handlingData) where
  getSchema _ = getSchema (Proxy @handlingData)
  toBs (MkSampledBy handlingData) = toBs handlingData
  fromBs bs = MkSampledBy $ fromBs bs


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


class Counter f where
  countOf :: f a -> Int

instance Counter V1 where
  countOf = const 0

instance Counter U1 where
  countOf = const 1

instance (Counter f1, Counter f2) => Counter (f1 :*: f2) where
  countOf (a :*: b) = countOf a + countOf b

instance Counter (K1 i c) where
  countOf = const 1

instance Counter f => Counter (M1 i t f) where
  countOf = countOf . unM1


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
    -- really need to optomize later
    let byteStrings = '\t' `split` bs
        lng = length byteStrings
        firstWordsCount = lng `div` 2
        lastWordsCount = lng - firstWordsCount
        firstWords = BS8.intercalate "\t" $ take firstWordsCount byteStrings
        lastWords = BS8.intercalate "\t" $ reverse $ take lastWordsCount $ reverse byteStrings in
    gFromBs firstWords
    :*:
    gFromBs lastWords
  {-# INLINE gFromBs #-}

instance (IsChType p)
  => GFromBS (S1 s (K1 i p)) where

  gFromBs :: IsChType p => ByteString -> S1 s (K1 i p) p1
  gFromBs bs = M1 $ K1 $ parse bs
  {-# INLINE gFromBs #-}
