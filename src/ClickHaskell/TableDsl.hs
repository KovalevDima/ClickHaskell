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

module ClickHaskell.TableDsl
  ( HasChSchema(..)
  
  , SampledBy
  , EqualityWith, Infixion

  , InDatabase

  , Table
  , DefaultColumn

  , IsChEngine
  , MergeTree, TinyLog

  , ToConditionalExpression(toConditionalExpression), SupportedAndVerifiedColumns, Unwraped
  , showCreateTableIfNotExists, showCreateTable

  , KnownSymbols, KnownTupleSymbols
  ) where

-- Internal dependencies
import ClickHaskell.ChTypes (IsChType(originalName, parse, render), ToChTypeName)

-- GHC included libraries imports
import Data.ByteString         as BS (ByteString)
import Data.ByteString.Char8   as BS8 (split, intercalate)
import Data.Data               (Proxy(Proxy))
import Data.Kind               (Type)
import Data.Text               as T (Text, pack, unpack, intercalate)
import Data.Text.Lazy.Builder  (Builder, fromString)
import GHC.Generics            (Generic(Rep, from, to), Selector(selName), (:*:)(..), D1, C1, S1, M1(..), K1(unK1, K1))
import GHC.TypeLits            (symbolVal, KnownSymbol, TypeError, ErrorMessage(..), Symbol)




type Unwraped :: Type -> Type
type family Unwraped t where
  Unwraped (SampledBy fieldName conditionalExpression handlingData) = Unwraped handlingData
  Unwraped handlingData = handlingData



 
class ToConditionalExpression t where
  toConditionalExpression :: Builder

instance
  ( ToConditionalExpression (SampledBy fieldName2 conditionalExpPart2 handlingData)
  , ToConditionalExpPart conditionalExpPart
  , ToConditionalExpPart conditionalExpPart2
  , KnownSymbol fieldName
  , KnownSymbol fieldName2
  ) => 
  ToConditionalExpression (SampledBy fieldName conditionalExpPart)
  where
  toConditionalExpression
    =  fromString (symbolVal (Proxy @fieldName))  <> "=" <> toConditionalExpPart @conditionalExpPart
    <> " AND "
    <> toConditionalExpression @(SampledBy fieldName2 conditionalExpPart2 handlingData)

instance
  ( HasChSchema handlingData
  , ToConditionalExpPart conditionalExpPart
  , KnownSymbol fieldName
  ) =>
  ToConditionalExpression (SampledBy fieldName conditionalExpPart handlingData)
  where
  toConditionalExpression = fromString (symbolVal (Proxy @fieldName)) <> "=" <> toConditionalExpPart @conditionalExpPart

instance {-# OVERLAPPABLE #-}
  ( HasChSchema handlingData
  ) =>
  ToConditionalExpression handlingData
  where
  toConditionalExpression = ""




class ToConditionalExpPart a where
  toConditionalExpPart :: Builder 


data SampledBy (fieldName :: Symbol) (conditionalExpression :: Type) handlingData where 
  MkSampledBy :: handlingData -> SampledBy fieldName conditionalExpression handlingData
  deriving (Generic, Show, Functor)


data EqualityWith (a :: Symbol)
instance KnownSymbol a
  => ToConditionalExpPart (EqualityWith a) where
  toConditionalExpPart = "='" <> fromString (symbolVal (Proxy @a)) <> "'"

data Infixion      a




type ValidatedRequest :: Type -> Table name columns c d e -> [(Symbol, Symbol)]
type family ValidatedRequest handlingDataDescripion table where
  ValidatedRequest handlingDataDescripion (t :: Table name columns c d e)
    = TransformedToSupportedColumns columns


type ValidatedSubset :: [(Symbol, Type)] -> [(Symbol, Type)] -> [(Symbol, Type)]
type family ValidatedSubset a b where
  ValidatedSubset ('(field, fieldType) ': cols1) cols2 = ColumnSolve '(field, fieldType) cols2 ': cols2


type ColumnSolve :: (Symbol, Type) -> [(Symbol, Type)] -> (Symbol, Type)
type family ColumnSolve a as where
  ColumnSolve '(field, fieldType) ('(field, fieldType) ': fields) = 
    '(field, fieldType)
  ColumnSolve '(field, fieldType1) ('(field, fieldType2) ': fields) =
    TypeError ('Text "There is a field " ':<>: 'Text field ':<>: 'Text " had duplicated type declarations")
  ColumnSolve '(field, _) '[] =
    TypeError ('Text "There is a field " ':<>: 'Text field ':<>: 'Text " that is out of table")
  ColumnSolve column (column' ': columns) = ColumnSolve column columns




type family SupportedAndVerifiedColumns (columns :: [Type]) :: [(Symbol, Symbol)] where
  SupportedAndVerifiedColumns xs = NoDuplicated (TransformedToSupportedColumns xs)

type NoDuplicated :: [(Symbol, Symbol)] -> [(Symbol, Symbol)]
type family NoDuplicated xs where
  NoDuplicated (x ': xs) = ElemOrNot x xs ': NoDuplicated xs
  NoDuplicated '[] = '[]

type ElemOrNot :: a -> [a] -> a
type family ElemOrNot a as where
  ElemOrNot a '[] = a
  ElemOrNot '(a, _) ('(a, _) ': xs) = 
    TypeError ('Text "There is a field " ':<>: 'Text a ':<>: 'Text " duplicated")
  ElemOrNot '(a, c) ('(b, _) ': xs) = ElemOrNot '(a, c) xs




type family TransformedToSupportedColumns (columns :: [Type]) :: [(Symbol, Symbol)] where
  TransformedToSupportedColumns (x ': '[]) = SupportedColumn x ': '[]
  TransformedToSupportedColumns (x ': xs)  = SupportedColumn x ': TransformedToSupportedColumns xs
  TransformedToSupportedColumns '[]        = TypeError ('Text "No columns in table")


type family SupportedColumn x :: (Symbol, Symbol) where
  SupportedColumn (DefaultColumn a b) = '(a, ToChTypeName b)




data InDatabase
  (db :: Symbol)
  (t :: Type)


data Table
  (name :: Symbol)
  (columns :: [column :: Type])
  engine
  (partitionBy :: [Symbol])
  (orderBy     :: [Symbol])


class KnownSymbols (ns :: [Symbol]) where symbolsVal :: [Text]
instance KnownSymbols '[] where symbolsVal = []
instance (KnownSymbol n, KnownSymbols ns) => KnownSymbols (n ': ns) where
  symbolsVal = T.pack (symbolVal (Proxy :: Proxy n)) : symbolsVal @ns

class KnownTupleSymbols (ns :: [(Symbol, Symbol)]) where
  symbolsTupleVals :: [(Text, Text)]
instance KnownTupleSymbols '[] where symbolsTupleVals = []
instance (KnownSymbol a, KnownSymbol b, KnownTupleSymbols ns) => KnownTupleSymbols ('(a,b) ': ns) where
  symbolsTupleVals = (T.pack (symbolVal (Proxy :: Proxy a)), T.pack (symbolVal (Proxy :: Proxy b))) : symbolsTupleVals @ns




data DefaultColumn (name :: Symbol) columnType




showCreateTableIfNotExists :: forall t db table name columns engine orderBy partitionBy .
  ( table ~ Table name columns engine orderBy partitionBy
  , t ~ InDatabase db table
  , KnownSymbol name
  , KnownSymbol db
  , KnownSymbols partitionBy
  , KnownSymbols orderBy
  , KnownTupleSymbols (SupportedAndVerifiedColumns columns)
  , IsChEngine engine
  ) => String
showCreateTableIfNotExists =
  let columns     = symbolsTupleVals @(SupportedAndVerifiedColumns columns)
      partitionBy = symbolsVal @partitionBy
      orderBy     = symbolsVal @orderBy
  in "CREATE TABLE IF NOT EXISTS "  <> symbolVal (Proxy @db) <> "." <> symbolVal (Proxy @name)
  <> " "              <> T.unpack ("(" <> T.intercalate ", " (map (\(first, second) -> first <> " " <> second) columns) <> ")")
  <> " Engine="       <> engineName @engine
  <> " PARTITION BY " <> (if null partitionBy then "tuple()" else T.unpack ("(" <> T.intercalate ", " partitionBy <> ")"))
  <> " ORDER BY "     <> (if null orderBy     then "tuple()" else T.unpack ("(" <> T.intercalate ", " orderBy     <> ")"))


showCreateTable :: forall t db table name columns engine orderBy partitionBy .
  ( table ~ Table name columns engine orderBy partitionBy
  , t ~ InDatabase db table
  , KnownSymbol name
  , KnownSymbol db
  , KnownSymbols partitionBy
  , KnownSymbols orderBy
  , KnownTupleSymbols (SupportedAndVerifiedColumns columns)
  , IsChEngine engine
  ) => String
showCreateTable =
  let columns     = symbolsTupleVals @(SupportedAndVerifiedColumns columns)
      partitionBy = symbolsVal @partitionBy
      orderBy     = symbolsVal @orderBy
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
  default getSchema :: (Generic a, GHasChSchema (Rep a)) => [(Text, Text)]
  getSchema :: [(Text, Text)]
  getSchema = toSchema @(Rep a)

  default toBs :: (Generic a, GToBs (Rep a)) => a -> BS.ByteString
  toBs :: a -> BS.ByteString
  toBs = (<> "\n") . gToBs . from
  {-# INLINE toBs #-}

  default fromBs :: (Generic a, GFromBS (Rep a)) => BS.ByteString -> a
  fromBs :: BS.ByteString -> a
  fromBs = to . gFromBs
  {-# INLINE fromBs #-}

instance (HasChSchema handlingData)
  => HasChSchema (SampledBy fieldName conditionalExpression handlingData) where
  getSchema :: HasChSchema handlingData => [(Text, Text)]
  getSchema = getSchema @handlingData

  toBs :: HasChSchema handlingData => SampledBy fieldName conditionalExpression handlingData -> ByteString
  toBs (MkSampledBy handlingData) = toBs handlingData

  fromBs :: HasChSchema handlingData => ByteString -> SampledBy fieldName conditionalExpression handlingData
  fromBs bs = MkSampledBy $ fromBs bs




class GHasChSchema (p :: Type -> Type)
  where toSchema :: [(Text, Text)]

instance (GHasChSchema f)
  => GHasChSchema (D1 c f) where
  toSchema = toSchema @f
instance (GHasChSchema f)
  => GHasChSchema (C1 c f) where
  toSchema = toSchema @f
instance (GHasChSchema f, GHasChSchema f2)
  => GHasChSchema (f :*: f2) where
  toSchema = toSchema @f <> toSchema @f2
instance (IsChType p, Selector s)
  => GHasChSchema (S1 s (f p)) where
  toSchema = [(T.pack $ selName (undefined :: t s f1 a), originalName (Proxy @p))]




class GToBs f where
  gToBs :: f p -> BS.ByteString

instance GToBs f
  => GToBs (D1 c f) where
  gToBs (M1 re) = gToBs re
  {-# INLINE gToBs #-}
instance GToBs f
  => GToBs (C1 c f) where
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
instance (IsChType p)
  => GFromBS (S1 s (K1 i p)) where
  gFromBs bs = M1 $ K1 $ parse bs
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
