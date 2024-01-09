{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DefaultSignatures
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
  , InstanceSigs
  , NamedFieldPuns
  , OverloadedStrings
  , PolyKinds
  , RankNTypes
  , TypeFamilyDependencies
  , UndecidableInstances
  , UndecidableSuperClasses
#-}

{-#
  OPTIONS_GHC -Wno-missing-methods
#-}

module ClickHaskell.Operations
  (
  -- * DSL
    IsOperation(..)
  , ClickHouse

  -- * Writing
  , renderInsertHeader
  , Writing
  , WritableInto(toTsvLine)

  -- * Reading
  , renderSelectQuery
  , Reading
  , ReadableFrom(fromTsvLine)
  ) where

-- Internal dependencies
import ClickHaskell.Operations.Generics (GReadable(..), GWritable(..))
import ClickHaskell.Tables              (IsTable(..))

-- GHC included libraries imports
import Data.ByteString         as BS (toStrict, StrictByteString)
import Data.ByteString.Builder as BS (Builder, toLazyByteString)
import Data.Kind               (Type)
import GHC.Generics            (Generic(..))
import GHC.TypeError           (TypeError, ErrorMessage(..))


-- * DSL

data ClickHouse a

class
  IsOperation description
  where
  operation :: Operation description
  operation = operationPart (operationArg @description)
  type Operation description = simplifiedDescription | simplifiedDescription -> description

  operationPart :: OperationArg description -> Operation description
  type OperationArg description :: Type
  operationArg :: OperationArg description


instance {-# OVERLAPPABLE #-}
  ( TypeError
    (    'Text "Expected an Operation DSL expression, but got:"
    :$$: ShowType description
    )
  ) => IsOperation description








-- * Writing

data Writing a

data WritingState a = MkWriting
  { writingColumns   :: Builder
  , writingTableName :: Builder
  }

renderInsertHeader :: WritingState a -> StrictByteString
renderInsertHeader MkWriting{writingColumns, writingTableName}
    = BS.toStrict . BS.toLazyByteString
      $  "INSERT INTO " <> writingTableName
      <> " (" <> writingColumns <> ")"


instance
  ( WritableInto table record
  ) =>
  IsOperation (Writing record -> ClickHouse table)
  where
  type OperationArg (Writing record -> ClickHouse table) = ()

  operationArg = ()

  type Operation (Writing record -> ClickHouse table)
    = WritingState (Writing record -> ClickHouse table)

  operationPart () = MkWriting
    { writingColumns   = renderedWritingColumns @table @record
    , writingTableName = mkTableName @table
    }


class
  ( IsTable table
  , Writable table
  ) => WritableInto table record where

  default toTsvLine
    ::
    ( GWritable (ValidateTable table) (GetTableColumns table) (Rep record)
    , Generic record
    ) => record -> BS.Builder
  toTsvLine :: record -> BS.Builder
  toTsvLine = gToTsvBs @(ValidateTable table) @(GetTableColumns table) . from
  {-# NOINLINE toTsvLine #-}

  default renderedWritingColumns
    ::
    ( GWritable (ValidateTable table) (GetTableColumns table) (Rep record)
    , Generic record
    ) => Builder
  renderedWritingColumns :: Builder
  renderedWritingColumns = gRenderedInsertableColumns @(ValidateTable table) @(GetTableColumns table) @(Rep record)
  {-# NOINLINE renderedWritingColumns #-}


instance  {-# OVERLAPPABLE #-}
  ( IsTable table
  , Writable table
  , Generic record
  , TypeError
    (    'Text "Derive WritableInto instance for your datatype:"
    :$$: 'Text "  |data " :<>: ShowType record
    :$$: 'Text "  |  { .."
    :$$: 'Text "  |  } deriving (Generic, WritableInto YourTableDescription)"
    )
  ) => WritableInto table record
  where
  toTsvLine _bs          = error "Unreachable"
  renderedWritingColumns = error "Unreachable"








-- * Reading

data Reading a

data ReadingState a = MkReading
  { readingColumns      :: Builder
  , readingTableName    :: Builder
  , renderedViewParameters :: [Builder]
  }

renderSelectQuery :: ReadingState a -> StrictByteString
renderSelectQuery MkReading{readingColumns, readingTableName, renderedViewParameters}
  = BS.toStrict
  . BS.toLazyByteString
    $  "SELECT " <> readingColumns
    <> " FROM " <> readingTableName <>
      if null renderedViewParameters
      then ""
      else "(" <> foldr ((<>) . (<> ", ")) (head renderedViewParameters) (tail renderedViewParameters) <> ")"

instance
  ( ReadableFrom table record
  ) =>
  IsOperation (Reading record -> ClickHouse table)
  where
  type OperationArg (Reading record -> ClickHouse table) = ()

  operationArg = ()
  
  type Operation (Reading record -> ClickHouse table)
    = ReadingState (Reading record -> ClickHouse table)
  
  operationPart () = MkReading
    { readingTableName       = mkTableName @table
    , readingColumns         = renderedReadingColumns @table @record
    , renderedViewParameters = []
    }


class
  ( IsTable table
  ) => ReadableFrom table record where

  default fromTsvLine
    ::
    ( GReadable (ValidateTable table) (GetTableColumns table) (Rep record)
    , Generic record
    ) => StrictByteString -> record
  fromTsvLine :: StrictByteString -> record
  fromTsvLine = to . gFromTsvBs @(ValidateTable table) @(GetTableColumns table)
  {-# NOINLINE fromTsvLine #-}

  default renderedReadingColumns
    ::
    ( GReadable (ValidateTable table) (GetTableColumns table) (Rep record)
    , Generic record
    ) => Builder
  renderedReadingColumns :: Builder
  renderedReadingColumns = gRenderedSelectableColumns @(ValidateTable table) @(GetTableColumns table) @(Rep record)
  {-# NOINLINE renderedReadingColumns #-}


instance {-# OVERLAPPABLE #-}
  ( IsTable table
  , Generic record
  , TypeError
    (    'Text "Derive ReadableFrom instance for your datatype:"
    :$$: 'Text "  |data " :<>: ShowType record
    :$$: 'Text "  |  { .."
    :$$: 'Text "  |  } deriving (Generic, ReadableFrom YourTableDescription)"
    )
  ) => ReadableFrom table record
  where
  fromTsvLine _bs        = error "Unreachable"
  renderedReadingColumns = error "Unreachable"
