{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  , MultiParamTypeClasses
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

module ClickHaskell.DataDsl.Inserting
  ( InsertableInto(toInsertableInto)
  
  , tsvInsertQueryHeader
  ) where

-- Internal dependencies
import ClickHaskell.DbTypes    (Serializable(serialize), ToChType(toChType))
import ClickHaskell.Validation (HandleErrors, GetGenericProductHeadSelector, SpanByColumnName, GetGenericProductLastSelector, AssumePlacedBefore)
import ClickHaskell.TableDsl   (InDatabase, IsTable(..), IsLocatedTable (..))

-- GHC included libraries imports
import Data.ByteString as BS (ByteString)
import Data.Kind       (Type)
import Data.Text       as T (Text, intercalate)
import GHC.Generics    (Generic, (:*:)(..), Meta(MetaSel), S1, C1, D1, Generic(..), M1 (..), K1, unK1, Rec0)
import GHC.TypeLits    (ErrorMessage (..), TypeError, Symbol)


class
  ( IsTable table
  ) => InsertableInto table insertableData where
  default toInsertableInto
    ::
    ( GInsertable
      (TableValidationResult table)
      (GetTableColumns table)
      (Rep insertableData)
    , Generic insertableData
    ) => insertableData -> BS.ByteString

  toInsertableInto :: insertableData -> BS.ByteString
  toInsertableInto
    = gToBs
      @(TableValidationResult table)
      @(GetTableColumns table)
    . from
  {-# INLINE toInsertableInto #-}


instance {-# OVERLAPPING #-}
  ( InsertableInto table insertableData
  ) => InsertableInto (InDatabase dbName table) insertableData
  where
  toInsertableInto = toInsertableInto @table


instance  {-# OVERLAPPABLE #-}
  ( IsTable table
  , Generic insertableData
  , TypeError
    (    'Text "You didn't provide"
    :$$: 'Text "  ( InsertableInto "
    :$$: 'Text "    (Table \"" :<>: 'Text (GetTableName table) :<>: 'Text "\" ...) "
    :$$: 'Text "    (" :<>: ShowType insertableData :<>: 'Text ")"
    :$$: 'Text "  )"
    :$$: 'Text "instance"
    :$$: 'Text "Derive it via:"
    :$$: 'Text "  |data " :<>: ShowType insertableData
    :$$: 'Text "  |  { .."
    :$$: 'Text "  |  } deriving (Generic)"
    :$$: 'Text "  |instance InsertableInto (Table \"" :<>: 'Text (GetTableName table)  :<>: 'Text "\" ...) " :<>: ShowType insertableData
    )
  ) => InsertableInto table insertableData
  where
  toInsertableInto = error "Unreachable"




tsvInsertQueryHeader :: forall locatedTable handlingDataDescripion .
  ( InsertableInto locatedTable handlingDataDescripion
  , IsLocatedTable locatedTable
  ) => Text
tsvInsertQueryHeader =
  let columnsMapping = T.intercalate "," $ getTableRenderedColumnsNames @locatedTable
  in "INSERT INTO " <> getDatabaseName @locatedTable <> "." <> getTableName @locatedTable
  <> " (" <> columnsMapping <> ")"
  <> " FORMAT TSV\n"




class GInsertable
  (deivingState :: (Bool, ErrorMessage))
  (columns :: [(Symbol, Type)])
  f
  where
  gToBs :: f p -> BS.ByteString


instance
  ( TypeError errorMsg
  ) => GInsertable '(True, errorMsg) columns genericRep where
  gToBs _ = error "Unreachable"
  {-# INLINE gToBs #-}


instance {-# OVERLAPPING #-}
  ( GInsertable '(False, unreachableError) columns f
  ) => GInsertable '(False, unreachableError) columns (D1 c f)
  where
  gToBs (M1 re) = gToBs @'(False, unreachableError) @columns re <> "\n"
  {-# INLINE gToBs #-}


instance {-# OVERLAPPING #-}
  ( GInsertable '(False, unreachableError) columns f
  ) => GInsertable '(False, unreachableError) columns (C1 c f)
  where
  gToBs (M1 re) = gToBs @'(False, unreachableError) @columns re
  {-# INLINE gToBs #-}


instance {-# OVERLAPPING #-}
  ( firstTreeElement ~ GetGenericProductHeadSelector left
  , leftCenterTreeElement ~ GetGenericProductLastSelector left
  , rightCenterTreeElement ~ GetGenericProductHeadSelector right
  , lastTreeElement ~ GetGenericProductLastSelector right
  , '(firstColumnsPart, secondColumnsPart) ~ SpanByColumnName rightCenterTreeElement columns
  , derivingState ~ HandleErrors
    '[ firstTreeElement       `AssumePlacedBefore` leftCenterTreeElement
     , leftCenterTreeElement  `AssumePlacedBefore` rightCenterTreeElement
     , rightCenterTreeElement `AssumePlacedBefore` lastTreeElement
     ]
  , GInsertable derivingState firstColumnsPart left
  , GInsertable derivingState secondColumnsPart right
  ) => GInsertable '(False, unreachableError) columns (left :*: right)
  where
  gToBs (left :*: right)
    =          gToBs @derivingState @firstColumnsPart left
    <> "\t" <> gToBs @derivingState @secondColumnsPart right
  {-# INLINE gToBs #-}


instance {-# OVERLAPPING #-}
  ( Serializable chType
  , ToChType chType inputType
  ) => GInsertable '(False, unrechableError) '[ '(columnName, chType)]
    ( S1 (MetaSel (Just columnName) a b f) (Rec0 inputType)
    )
  where
  gToBs = serialize . toChType @chType @inputType . unK1 . unM1
  {-# INLINE gToBs #-}


instance
  ( TypeError
    (    'Text "Column " :<>: ShowType someElem :<>: 'Text " required for insert."
    :$$: 'Text "Add it to your insertable type"
    )
  ) => GInsertable '(False, unrechableError) ('(otherColumnName, chType) ': someElem ': moreColumns) (S1 columnName (K1 i inputType))
  where
  gToBs _ = error "Unreachable"


instance
  ( TypeError
    (    'Text "There is no column " :<>: 'Text columnName :<>: 'Text " in table"
    :$$: 'Text "You can't insert this field"
    )
  ) => GInsertable '(False, unrechableError) '[] (S1 (MetaSel (Just columnName) a b f) (K1 i inputType))
  where
  gToBs = error "Unreachable"
