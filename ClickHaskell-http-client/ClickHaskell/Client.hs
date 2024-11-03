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
  , UndecidableInstances
  , UndecidableSuperClasses 
  , LambdaCase
  , DeriveAnyClass
  , DeriveGeneric
#-}

{-# OPTIONS_GHC
  -Wno-orphans
#-}

module ClickHaskell.Client
  ( select
  , selectFrom
  , selectFromView
  , selectFromTableFunction

  , insertInto

  , runStatement

  , WritableInto(..)
  , ReadableFrom(..)
  , Deserializable(..)

  , ChCredential(..)
  ) where

-- Internal
import ClickHaskell.Tables
import ClickHaskell.DbTypes

-- External

-- GHC included
import Control.DeepSeq (NFData)
import Control.Exception (Exception, SomeException, throw)
import Data.ByteString as BS (StrictByteString, drop, empty, take, toStrict)
import Data.ByteString.Builder as BS (Builder, byteString, toLazyByteString, int16Dec, int8Dec, int32Dec, int64Dec, integerDec, word8Dec, word16Dec, word32Dec, word64Dec)
import Data.ByteString.Char8 as BS8 (break, concatMap, length, pack, readInt, readInteger, replicate, singleton, span, unpack)
import Data.ByteString.Lazy as BSL (fromChunks, toChunks)
import Data.ByteString.Lazy.Char8 as BSL8 (lines)
import Data.IORef (atomicModifyIORef, newIORef, readIORef, writeIORef)
import Data.Int
import Data.Kind (Constraint, Type)
import Data.Maybe (fromJust)
import Data.Text as T (Text, unpack)
import Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Typeable (Proxy (..))
import GHC.Generics
import GHC.IO (unsafeInterleaveIO)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, TypeError, symbolVal)
import Data.Type.Bool (If)

-- External
import qualified Data.UUID as UUID
import Data.Word
import Network.HTTP.Client as H (BodyReader, Manager, Request (..), RequestBody (..), Response (..), brConsume, parseRequest, withResponse)
import Network.HTTP.Types as H (Status (..))


insertInto ::
  forall table record name columns
  .
  ( WritableInto table record
  , KnownSymbol name
  , table ~ Table name columns
  )
  => Manager -> ChCredential -> [record] -> IO ()
insertInto manager cred writingData = do
  insertIntoHttpGeneric
    cred
    ("INSERT INTO " <> (byteString . BS8.pack) (symbolVal $ Proxy @name) <> " (" <> writingColumns @table @record <> ") FORMAT TSV\n")
    (toTsvLine @table @record)
    writingData
    (`withResponse` manager)

selectFrom ::
  forall table record name columns
  .
  ( ReadableFrom table record
  , KnownSymbol name
  , table ~ Table name columns
  )
  => Manager -> ChCredential -> IO [record]
selectFrom manager cred =
  selectFromHttpGeneric
    @record
    cred
    ("SELECT " <> readingColumns @table @record <> " FROM " <> (byteString . BS8.pack) (symbolVal $ Proxy @name) <> " FORMAT TSV\n")
    (fromTsvLine @table @record)
    (`withResponse` manager)

selectFromView ::
  forall tableFunction record name columns parameters passedParameters
  .
  ( ReadableFrom tableFunction record
  , KnownSymbol name
  , tableFunction ~ View name columns parameters
  , CheckParameters parameters passedParameters
  )
  => Manager -> ChCredential -> (Parameters '[] -> Parameters passedParameters) -> IO [record]
selectFromView manager cred interpreter =
  selectFromHttpGeneric
    @record
    cred
    ( "SELECT " <> readingColumns @tableFunction @record <>
      " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> parameters interpreter <>
      " FORMAT TSV\n")
    (fromTsvLine @tableFunction @record)
    (`withResponse` manager)

{-# DEPRECATED selectFromTableFunction "selectFromTableFunction would be deleted soon due wrong naming. Use selectFromView instead" #-}
selectFromTableFunction ::
  forall tableFunction record name columns parameters passedParameters
  .
  ( ReadableFrom tableFunction record
  , KnownSymbol name
  , tableFunction ~ View name columns parameters
  , CheckParameters parameters passedParameters
  )
  => Manager -> ChCredential -> (Parameters '[] -> Parameters passedParameters) -> IO [record]
selectFromTableFunction manager cred interpreter =
  selectFromHttpGeneric
    @record
    cred
    ( "SELECT " <> readingColumns @tableFunction @record <>
      " FROM " <> (byteString . BS8.pack . symbolVal @name) Proxy <> parameters interpreter <>
      " FORMAT TSV\n")
    (fromTsvLine @tableFunction @record)
    (`withResponse` manager)

select ::
  forall (columns :: [Type]) record
  .
  ReadableFrom columns record
  => Manager -> ChCredential -> Builder -> IO [record]
select manager cred query =
  selectFromHttpGeneric
    @record
    cred
    (query <> " FORMAT TSV\n")
    (fromTsvLine @columns @record)
    (`withResponse` manager)

runStatement :: Manager -> ChCredential -> Builder -> IO StrictByteString
runStatement manager chCred statement = injectStatementToRequest
  statement
  (either throw id $ initAuthorizedRequest chCred)
  `withResponse` manager $ fmap mconcat . brConsume . responseBody


{- | ToDocument
-}
data ChCredential = MkChCredential
  { chLogin    :: !Text
  , chPass     :: !Text
  , chUrl      :: !Text
  , chDatabase :: !Text
  }
  deriving (Generic, NFData, Show, Eq)

{- | ToDocument
-}
newtype ChException = MkChException
  { exceptionMessage :: Text
  }
  deriving (Show)
  deriving anyclass (Exception)

--
--
-- * Deprecated
--
--

insertIntoHttpGeneric
  :: forall record
  .  ChCredential
  -> Builder
  -> (record -> Builder)
  -> [record]
  -> (Request -> (forall result. (Response BodyReader -> IO result) -> IO result))
  -> IO ()
insertIntoHttpGeneric credential query encoder records runClient = injectWritingToRequest
  query
  records
  encoder
  (either throw id $ initAuthorizedRequest credential)
  `runClient` \response -> do
    _ <- throwOnNon200  response
    pure ()

selectFromHttpGeneric
  :: forall record
  .  ChCredential
  -> Builder
  -> (StrictByteString -> record)
  -> (Request -> (forall result . (Response BodyReader -> IO result) -> IO result))
  -> IO [record]
selectFromHttpGeneric credential query decoder runClient =
  (injectStatementToRequest query . either throw id)
  (initAuthorizedRequest credential)
  `runClient` \response -> do
    _ <- throwOnNon200 response
    injectReadingToResponse
      decoder
      response

-- ToDo: This implementation reads whole body before parsing
injectReadingToResponse :: (StrictByteString -> record) -> (Response BodyReader -> IO [record])
injectReadingToResponse decoder = fmap (map (decoder . toStrict) . BSL8.lines  . BSL.fromChunks) . brConsume . unsafeInterleaveIO . responseBody


injectWritingToRequest :: Builder -> [rec] -> (rec -> Builder) -> (Request -> Request)
injectWritingToRequest query dataList encoder request = request{
    requestBody = RequestBodyStreamChunked $ \np -> do
      writingData <- newIORef . BSL.toChunks . toLazyByteString . mconcat . (query:) . map encoder $ dataList
      np . atomicModifyIORef writingData $ \case
        [] -> ([], BS.empty)
        x:xs -> (xs, x)
  }


throwOnNon200 :: Response BodyReader -> IO (Response BodyReader)
throwOnNon200 resp = do
    if H.statusCode (responseStatus resp) /= 200
      then throw . MkChException . T.decodeUtf8 . mconcat =<< (brConsume . responseBody) resp
      else pure resp


injectStatementToRequest :: Builder -> Request -> Request
injectStatementToRequest query request = request{
    requestBody = RequestBodyStreamChunked $ \np -> do
      ibss <- newIORef $ (BSL.toChunks . toLazyByteString) query
      np $ do
        bss <- readIORef ibss
        case bss of
          [] -> return BS.empty
          bs:bss' -> do
            writeIORef ibss bss'
            return bs
  }

initAuthorizedRequest :: ChCredential -> Either SomeException Request
initAuthorizedRequest (MkChCredential login pass url databaseName) = do
    req <- H.parseRequest (T.unpack url)
    pure $!
      req
        { H.method         = "POST"
        , H.requestHeaders =
          [ ("X-ClickHouse-User", encodeUtf8 login)
          , ("X-ClickHouse-Key", encodeUtf8 pass)
          , ("X-ClickHouse-Database", encodeUtf8 databaseName)
          ]
          <> H.requestHeaders req
        }


-- ** Reading

class
  ( HasColumns hasColumns
  , GReadable (GetColumns hasColumns) (Rep record)
  ) =>
  ReadableFrom hasColumns record
  where

  default fromTsvLine :: (Generic record) => StrictByteString -> record
  fromTsvLine :: StrictByteString -> record
  fromTsvLine = to . gFromTsvBs @(GetColumns hasColumns)

  default readingColumns :: (Generic record) => Builder
  readingColumns :: Builder
  readingColumns = gReadingColumns @(GetColumns hasColumns) @(Rep record)

class GReadable
  (columns :: [Type])
  f
  where
  gFromTsvBs :: StrictByteString -> f p
  gReadingColumns :: Builder

instance
  GReadable columns f
  =>
  GReadable columns (D1 c (C1 c2 f))
  where
  gFromTsvBs = M1 . M1 . gFromTsvBs @columns
  gReadingColumns = gReadingColumns @columns @f


instance
  GReadable columns (left1 :*: (left2 :*: right))
  =>
  GReadable columns ((left1 :*: left2) :*: right)
  where
  gFromTsvBs bs =
    let (left1 :*: (left2 :*: right)) = gFromTsvBs @columns bs
    in ((left1 :*: left2) :*: right)
  gReadingColumns = gReadingColumns @columns @(left1 :*: (left2 :*: right))

instance
  ( KnownColumn column
  , '(column, restColumns) ~ TakeColumn selectorName columns
  , FromChType (GetColumnType column) inputType
  , Deserializable (GetColumnType column)
  , GReadable restColumns right
  ) => GReadable columns (S1 (MetaSel (Just selectorName) a b f) (Rec0 inputType) :*: right)
  where
  gFromTsvBs bs =
    let (beforeTab, afterTab) = BS8.span (/= '\t') bs
    in
    (M1 . K1 . fromChType @(GetColumnType column) . deserialize $ beforeTab) :*: gFromTsvBs @restColumns @right (BS.drop 1 afterTab)
  gReadingColumns = renderColumnName @column <> ", " <> gReadingColumns @restColumns @right

instance
  ( KnownColumn column
  , '(column, restColumns) ~ TakeColumn selectorName columns
  , Deserializable (GetColumnType column)
  , FromChType (GetColumnType column) inputType
  ) => GReadable columns ((S1 (MetaSel (Just selectorName) a b f)) (Rec0 inputType))
  where
  gFromTsvBs = M1 . K1 . fromChType @(GetColumnType column) . deserialize
  gReadingColumns = renderColumnName @column




-- *** Deserialization

class
  Deserializable chType
  where
  deserialize :: StrictByteString -> chType

instance
  Deserializable chType
  =>
  Deserializable (Nullable chType)
  where
  deserialize "\\N" = Nothing
  deserialize someTypeBs = Just (deserialize someTypeBs)

instance
  ( Deserializable chType
  , ToChType chType chType
  , IsLowCardinalitySupported chType
  ) =>
  Deserializable (LowCardinality chType)
  where
  deserialize = toChType @(LowCardinality chType) @chType . deserialize

instance Deserializable ChUUID
  where
  deserialize = toChType . fromJust . UUID.fromASCIIBytes

instance Deserializable ChString
  where
  deserialize = toChType . deescape

-- There are a big trade off between safity and performance
-- Corner case strings with a lot of escaped symbols would reduce deserialization speed
-- ToDo: rewrite (de)serialization to work via binary clickhouse formats
deescape :: StrictByteString -> StrictByteString
deescape bs = case BS8.break (=='\\') bs of
  (beforeEscaping, startWithEscaping) ->
    if BS.empty == startWithEscaping
    then bs
    else case BS.take 2 startWithEscaping of
      "\\b" -> beforeEscaping <> "\b" <> BS.drop 2 startWithEscaping
      "\\t" -> beforeEscaping <> "\t" <> BS.drop 2 startWithEscaping
      "\\n" -> beforeEscaping <> "\n" <> BS.drop 2 startWithEscaping
      "\\f" -> beforeEscaping <> "\f" <> BS.drop 2 startWithEscaping
      "\\r" -> beforeEscaping <> "\r" <> BS.drop 2 startWithEscaping
      "\\'" -> beforeEscaping <> "'" <> BS.drop 2 startWithEscaping
      "\\\\" -> beforeEscaping <> "\\" <> BS.drop 2 startWithEscaping
      _ -> bs

instance Deserializable ChInt8
  where
  deserialize = toChType @ChInt8 @Int8 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChInt16
  where
  deserialize  = toChType @ChInt16 @Int16 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChInt32
  where
  deserialize = toChType @ChInt32 @Int32 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChInt64
  where
  deserialize = toChType @ChInt64 @Int64 . fromInteger . fst . fromJust . BS8.readInteger

instance Deserializable ChInt128
  where
  deserialize = toChType @ChInt128 @Int128 . fromInteger . fst . fromJust . BS8.readInteger

instance Deserializable ChUInt8
  where
  deserialize = toChType @ChUInt8 @Word8 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChUInt16
  where
  deserialize = toChType @ChUInt16 @Word16 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChUInt32
  where
  deserialize = toChType @ChUInt32 @Word32 . fromIntegral . fst . fromJust . BS8.readInt

instance Deserializable ChUInt64
  where
  deserialize = toChType @ChUInt64 @Word64 . fromIntegral . fst . fromJust . BS8.readInteger

instance Deserializable ChUInt128
  where
  deserialize = toChType @ChUInt128 @Word128 . fromIntegral . fst . fromJust . BS8.readInteger

instance Deserializable ChDateTime where
  deserialize
    = toChType @ChDateTime @Word32 . fromInteger
    . floor . utcTimeToPOSIXSeconds
    . fromJust . parseTimeM False defaultTimeLocale "%Y-%m-%d %H:%M:%S"
    . BS8.unpack

-- ** Writing

class
  ( HasColumns table
  , GWritable (GetColumns table) (Rep record)
  )
  =>
  WritableInto table record
  where
  default toTsvLine :: (Generic record) => record -> Builder
  toTsvLine :: record -> Builder
  toTsvLine = gToTsvBs @(GetColumns table) . from

  default writingColumns :: Builder
  writingColumns :: Builder
  writingColumns = gWritingColumns @(GetColumns table) @(Rep record)


class GWritable
  (columns :: [Type])
  f
  where
  gToTsvBs :: f p -> Builder
  gWritingColumns :: Builder

instance
  GWritable columns f
  =>
  GWritable columns (D1 c (C1 c2 f))
  where
  gToTsvBs (M1 (M1 re)) = gToTsvBs @columns re <> "\n"
  gWritingColumns = gWritingColumns @columns @f

instance
  GWritable columns (left1 :*: (left2 :*: right))
  =>
  GWritable columns ((left1 :*: left2) :*: right)
  where
  gToTsvBs ((left1 :*: left2) :*: right) = gToTsvBs @columns (left1 :*: (left2 :*: right))
  gWritingColumns = gWritingColumns @columns @(left1 :*: (left2 :*: right))

instance
  ( Serializable (GetColumnType column)
  , ToChType (GetColumnType column) inputType
  , KnownColumn column
  , GWritable restColumns right
  , GWritable '[column] ((S1 (MetaSel (Just typeName) a b f)) (Rec0 inputType))
  , '(column, restColumns) ~ TakeColumn typeName columns
  )
  =>
  GWritable columns ((S1 (MetaSel (Just typeName) a b f)) (Rec0 inputType) :*: right)
  where
  gToTsvBs (M1 (K1 dataType) :*: right)
    =  (serialize . toChType @(GetColumnType column)) dataType
    <> "\t"
    <> gToTsvBs @restColumns right
  gWritingColumns = renderColumnName @column <> ", " <> gWritingColumns @restColumns @right

instance
  ( ThereIsNoWriteRequiredColumns restColumns
  , Serializable (GetColumnType column)
  , ToChType (GetColumnType column) inputType
  , KnownColumn column
  , '(column, restColumns) ~ TakeColumn typeName columns
  ) =>
  GWritable columns (S1 (MetaSel (Just typeName) a b f) (Rec0 inputType))
  where
  gToTsvBs = serialize . toChType @(GetColumnType column) @inputType . unK1 . unM1
  gWritingColumns = renderColumnName @column


type family ThereIsNoWriteRequiredColumns (columns :: [Type]) :: Constraint where
  ThereIsNoWriteRequiredColumns '[] = ()
  ThereIsNoWriteRequiredColumns (column ': columns) =
    If
      (WriteOptionalColumn column)
      (ThereIsNoWriteRequiredColumns columns)
      (TypeError ('Text "Column " :<>: 'Text (GetColumnName column) :<>: 'Text " is required for insert but is missing"))


-- ** HasColumns helper class

class HasColumns (hasColumns :: k) where type GetColumns hasColumns :: [Type]
instance HasColumns (View name columns params) where type GetColumns (View _ columns _) = columns
instance HasColumns (Table name columns) where type GetColumns (Table _ columns) = columns
instance HasColumns (columns :: [Type]) where type GetColumns columns = columns




-- * Serialization

class
  Serializable chType
  where
  serialize :: chType -> Builder

instance
  Serializable chType
  =>
  Serializable (Nullable chType)
  where
  serialize = maybe "\\N" serialize

instance
  ( Serializable chType
  , FromChType chType chType
  , IsLowCardinalitySupported chType
  ) =>
  Serializable (LowCardinality chType)
  where
  serialize = serialize @chType . fromChType @(LowCardinality chType)

instance Serializable ChUUID
  where
  serialize = BS.byteString . UUID.toASCIIBytes . fromChType

instance Serializable ChString
  where
  serialize = (BS.byteString . escape) . fromChType

escape :: StrictByteString -> StrictByteString
escape -- [ClickHaskell.DbTypes.ToDo.2]: Optimize
  = BS8.concatMap
    (\case
      '\t' -> "\\t"
      '\n' -> "\\n"
      '\\' -> "\\\\"
      sym -> BS8.singleton sym
    )

instance Serializable ChInt8
  where
  serialize = BS.int8Dec . fromChType

instance Serializable ChInt16
  where
  serialize = BS.int16Dec . fromChType 

instance Serializable ChInt32
  where
  serialize = BS.int32Dec . fromChType

instance Serializable ChInt64
  where
  serialize = BS.int64Dec . fromChType

instance Serializable ChInt128
  where
  serialize = BS.integerDec . toInteger

instance Serializable ChUInt8
  where
  serialize = BS.word8Dec . fromChType

instance Serializable ChUInt16
  where
  serialize = BS.word16Dec . fromChType

instance Serializable ChUInt32
  where
  serialize = BS.word32Dec . fromChType

instance Serializable ChUInt64
  where
  serialize = BS.word64Dec . fromChType

instance Serializable ChUInt128
  where
  serialize = BS.integerDec . toInteger

instance Serializable ChDateTime where
  serialize chDateTime
    = let time = BS8.pack . show . fromChType @ChDateTime @Word32 $ chDateTime
    in BS.byteString (BS8.replicate (10 - BS8.length time) '0' <> time)


