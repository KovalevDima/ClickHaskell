{- |
  Module:      ClickHaskell
  Copyright:   (c) 2023 Dmitry Kovalev
  License:     BSD-3-Clause
  Maintainer:  Dmitry Kovalev
  Stability:   Experimental

  For full documentation, visit: https://clickhaskell.dev/
-}

module ClickHaskell
  (
  {- * Connection -}
    ConnectionArgs, defaultConnectionArgs
  , setHost, setPort, setUser, setDatabase, setPassword
  , overrideNetwork, overrideHostname, overrideOsUser
  , Connection(..), openConnection
  , Buffer(..)

  {- * Errors -}
  , ClientError(..)
  , ConnectionError(..)
  , UserError(..)
  , InternalError(..)

  {- * Client wrappers -}
  {- ** SELECT -}
  , select, selectFrom, selectFromView, generateRandom
  , ClickHaskell(..)
  {- ** INSERT -}
  , insertInto
  , ToChType(toChType, fromChType)
  {- ** Arbitrary commands -}, command, ping
  {- ** Shared -}
  , Column, KnownColumn, SerializableColumn
  , Table, View
  {- *** Query -}
  , ToQueryPart(toQueryPart), parameter, Parameter, Parameters, viewParameters

  {- * ClickHouse types -}
  , IsChType(chTypeName, defaultValueOfTypeName)
  , DateTime(..), DateTime64
  , Int8, Int16, Int32, Int64, Int128(..)
  , UInt8, UInt16, UInt32, UInt64, UInt128, Word128(..)
  , Nullable
  , LowCardinality, IsLowCardinalitySupported
  , UUID(..)
  , Array(..)
  , ChString(..)


  {- * Protocol parts -}

  {- ** Shared -}
  , UVarInt(..), SinceRevision(..), ProtocolRevision
  {- *** Data packet -}, DataPacket(..), BlockInfo(..)

  {- ** Client -}, ClientPacket(..)
  {- *** Hello -}, HelloPacket(..), Addendum(..)
  {- *** Query -}
  , QueryPacket(..)
  , DbSettings(..), QueryParameters(..), QueryStage(..)
  , ClientInfo(..), QueryKind(..)
  
  {- ** Server -}, ServerPacket(..)
  {- *** Hello -}, HelloResponse(..), PasswordComplexityRules(..)
  {- *** Exception -}, ExceptionPacket(..)
  {- *** Progress -}, ProgressPacket(..)
  {- *** ProfileInfo -}, ProfileInfo(..)
  {- *** TableColumns -}, TableColumns(..)
  ) where

-- Internal
import ClickHaskell.Columns
import ClickHaskell.Connection
import ClickHaskell.Packets
import ClickHaskell.Primitive
import ClickHaskell.Statements

-- GHC included
import Control.Applicative (liftA2)
import Control.Concurrent (newMVar, putMVar, takeMVar)
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, mask, onException, throw, throwIO)
import Control.Monad (when, (<$!>))
import Data.Binary.Get
import Data.ByteString as BS (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.ByteString.Lazy as BSL (toStrict)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (C1, D1, Generic (..), K1 (K1, unK1), M1 (M1, unM1), Meta (MetaSel), Rec0, S1, type (:*:) (..))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Prelude hiding (liftA2)

-- External
import Data.WideWord (Int128 (..), Word128 (..))
import Network.Socket hiding (SocketOption (..))

readBuffer :: Buffer -> Get a -> IO a
readBuffer buffer deseralize =
  catch @InternalError
    (rawBufferRead buffer deseralize)
    (throwIO . InternalError)

withConnection :: HasCallStack => Connection -> (ConnectionState -> IO a) -> IO a
withConnection (MkConnection connStateMVar) f =
  mask $ \restore -> do
    connState <- takeMVar connStateMVar
    b <- onException
      (restore (f connState))
      (putMVar connStateMVar =<< reopenConnection connState)
    putMVar connStateMVar connState
    return b

openConnection :: HasCallStack => ConnectionArgs -> IO Connection
openConnection creds@MkConnectionArgs{mHostname, mOsUser} = do
  hostname <- maybe (lookupEnv "HOSTNAME") (pure . Just) mHostname
  osUser   <- maybe (lookupEnv "USER")     (pure . Just) mOsUser
  connectionState <-
    createConnectionState
      . (maybe id overrideHostname hostname)
      . (maybe id overrideOsUser osUser)
      $ creds
  MkConnection <$> newMVar connectionState 

reopenConnection :: ConnectionState -> IO ConnectionState
reopenConnection MkConnectionState{creds, buffer} = do
  flushBuffer buffer
  closeSock buffer
  createConnectionState creds

createConnectionState :: ConnectionArgs -> IO ConnectionState
createConnectionState creds@MkConnectionArgs {user, pass, db, host, mPort, initBuffer, isTLS} = do
  let port = fromMaybe (if isTLS then "9440" else "9000") mPort
  AddrInfo{addrFamily, addrSocketType, addrProtocol, addrAddress}
    <- maybe (throwIO NoAdressResolved) pure . listToMaybe
    =<< getAddrInfo
      (Just defaultHints{addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream})
      (Just host)
      (Just port)
  buffer <- maybe (throwIO EstablishTimeout) pure
    =<< timeout 3_000_000 (
      bracketOnError
        (socket addrFamily addrSocketType addrProtocol)
        (\sock ->
          catch @SomeException
            (finally (shutdown sock ShutdownBoth) (close sock))
            (const $ pure ())
        )
        (\sock -> initBuffer host addrAddress sock)
      )

  (writeSock buffer . seriliazeHelloPacket db user pass) latestSupportedRevision
  serverPacketType <- readBuffer buffer (deserialize latestSupportedRevision)
  case serverPacketType of
    HelloResponse MkHelloResponse{server_revision} -> do
      let revision = min server_revision latestSupportedRevision
          conn = MkConnectionState{..}
      writeToConnection conn (\rev -> serialize rev MkAddendum{quota_key = MkSinceRevision ""})
      pure conn
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


{- |
  Might be used for any command without data responses

  For example: CREATE, TRUNCATE, KILL, SET, GRANT

  __Throws exception if any data was returned__
-}
command :: HasCallStack => Connection -> ChString -> IO ()
command conn query = do
  withConnection conn $ \connState -> do
    writeToConnection connState (serializeQueryPacket connState query)
    writeToConnection connState (serializeDataPacket "" 0 0)
    handleCreate connState
  where
  handleCreate :: ConnectionState -> IO ()
  handleCreate MkConnectionState{..} =
    readBuffer buffer (deserialize revision)
    >>= \packet -> case packet of
      EndOfStream         -> pure ()
      Exception exception -> throwIO (DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


-- * Ping

ping :: HasCallStack => Connection -> IO ()
ping conn = do
  withConnection conn $ \connState@MkConnectionState{revision, buffer} -> do
    writeToConnection connState (\rev -> serialize rev Ping)
    responsePacket <- readBuffer buffer (deserialize revision)
    case responsePacket of
      Pong                -> pure ()
      Exception exception -> throwIO (DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)




-- * Client wrappers

-- ** SELECT

select ::
  forall columns output result
  .
  ClickHaskell columns output
  =>
  Connection -> ChString -> ([output] -> IO result) -> IO [result]
select conn query f = do
  withConnection conn $ \connState -> do
    writeToConnection connState (serializeQueryPacket connState query)
    writeToConnection connState (serializeDataPacket "" 0 0)
    handleSelect @columns connState (\x -> id <$!> f x)

selectFrom ::
  forall table output result
  .
  ClickHaskellTable table output
  =>
  Connection -> ([output] -> IO result) -> IO [result]
selectFrom conn f = select @(GetColumns table) conn query f
  where
  query = toChType $
    "SELECT " <> columns @(GetColumns table) @output <>
    " FROM " <> tableName @table

selectFromView ::
  forall view output result parameters
  .
  ClickHaskellView view output
  =>
  Connection -> (Parameters '[] -> Parameters parameters) -> ([output] -> IO result) -> IO [result]
selectFromView conn interpreter f = select @(GetColumns view) conn query f
  where
  query = toChType $
    "SELECT " <> columns @(GetColumns view) @output <>
    " FROM " <> tableName @view <> viewParameters interpreter

generateRandom ::
  forall columns output result
  .
  ClickHaskell columns output
  =>
  Connection -> (UInt64, UInt64, UInt64) -> UInt64 -> ([output] -> IO result) -> IO [result]
generateRandom conn (randomSeed, maxStrLen, maxArrayLen) limit f = select @columns conn query f
  where
  query = toChType $
    "SELECT * FROM generateRandom(" <>
        "'" <> readingColumnsAndTypes @columns @output <> "' ," <>
          toQueryPart randomSeed <> "," <>
          toQueryPart maxStrLen <> "," <>
          toQueryPart maxArrayLen <>
      ")" <>
    " LIMIT " <> toQueryPart limit <> ";"

-- | Internal
handleSelect ::
  forall columns output result
  .
  ClickHaskell columns output
  =>
  ConnectionState -> ([output] -> IO result) -> IO [result]
handleSelect MkConnectionState{..} f = loopSelect []
  where
  loopSelect acc =
    readBuffer buffer (deserialize revision)
    >>= \packet -> case packet of
      DataResponse MkDataPacket{columns_count = 0, rows_count = 0} -> loopSelect acc
      DataResponse MkDataPacket{columns_count, rows_count} -> do
        let expected = columnsCount @columns @output
        when (columns_count /= expected) $
          (throw . UnmatchedResult . UnmatchedColumnsCount)
            ("Expected " <> show expected <> " columns but got " <> show columns_count)
        result <- f =<< readBuffer buffer (deserializeRecords @columns True revision rows_count)
        loopSelect (result : acc)
      Progress    _       -> loopSelect acc
      ProfileInfo _       -> loopSelect acc
      EndOfStream         -> pure acc
      Exception exception -> throwIO (DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


-- ** INSERT

insertInto ::
  forall table record
  .
  ClickHaskellTable table record
  =>
  Connection -> [record] -> IO ()
insertInto conn columnsData = do
  withConnection conn $ \connState -> do
    writeToConnection connState (serializeQueryPacket connState query)
    writeToConnection connState (serializeDataPacket "" 0 0)
    handleInsertResult @(GetColumns table) connState columnsData
  where
  query = toChType $
    "INSERT INTO " <> tableName @table
    <> " (" <> columns @(GetColumns table) @record <> ") VALUES"

-- | Internal
handleInsertResult :: forall columns record . ClickHaskell columns record => ConnectionState -> [record] -> IO ()
handleInsertResult conn@MkConnectionState{..} records = do
  firstPacket <- readBuffer buffer (deserialize revision)
  case firstPacket of
    TableColumns      _ -> handleInsertResult @columns conn records
    DataResponse MkDataPacket{} -> do
      _emptyDataPacket <- readBuffer buffer (deserializeRecords @columns @record False revision 0)
      let rows = fromIntegral (Prelude.length records)
          cols = columnsCount @columns @record
      writeToConnection conn (serializeDataPacket "" cols rows)
      writeToConnection conn (serializeRecords @columns records)
      writeToConnection conn (serializeDataPacket "" 0 0)
      handleInsertResult @columns @record conn []
    EndOfStream         -> pure ()
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)

type ClickHaskellTable table record =
  ( IsTable table
  , ClickHaskell (GetColumns table) record
  )

type ClickHaskellView view record =
  ( IsView view
  , ClickHaskell (GetColumns view) record
  )

serializeQueryPacket :: ConnectionState -> ChString -> (ProtocolRevision -> Builder)
serializeQueryPacket MkConnectionState{creds=MkConnectionArgs{..}, ..} query =
  flip serialize $ Query
    MkQueryPacket
      { query_id = ""
      , client_info                    = MkSinceRevision MkClientInfo
        { query_kind                   = InitialQuery
        , initial_user                 = toChType user
        , initial_query_id             = ""
        , initial_adress               = "0.0.0.0:0"
        , initial_time                 = MkSinceRevision 0
        , interface_type               = 1 -- [tcp - 1, http - 2]
        , os_user                      = maybe "" toChType mOsUser
        , hostname                     = maybe "" toChType mHostname
        , client_name                  = clientName
        , client_version_major         = major
        , client_version_minor         = minor
        , client_revision              = revision
        , quota_key                    = MkSinceRevision ""
        , distrubuted_depth            = MkSinceRevision 0
        , client_version_patch         = MkSinceRevision patch
        , open_telemetry               = MkSinceRevision 0
        , collaborate_with_initiator   = MkSinceRevision 0
        , count_participating_replicas = MkSinceRevision 0
        , number_of_current_replica    = MkSinceRevision 0
        }
      , settings           = MkDbSettings
      , interserver_secret = MkSinceRevision ""
      , query_stage        = Complete
      , compression        = 0
      , query
      , parameters         = MkSinceRevision MkQueryParameters
      }

seriliazeHelloPacket :: String -> String -> String -> (ProtocolRevision -> Builder)
seriliazeHelloPacket db user pass =
  flip serialize $ Hello
    MkHelloPacket
      { client_name          = clientName
      , client_version_major = major
      , client_version_minor = minor
      , tcp_protocol_version = latestSupportedRevision
      , default_database     = toChType db
      , user                 = toChType user
      , pass                 = toChType pass
      }

serializeDataPacket :: ChString -> UVarInt -> UVarInt -> (ProtocolRevision -> Builder)
serializeDataPacket table_name columns_count rows_count =
  flip serialize $ Data
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








-- * Errors handling

{- |
  A wrapper for all client-related errors
-}
data ClientError where
  UnmatchedResult :: HasCallStack => UserError -> ClientError
  DatabaseException :: HasCallStack => ExceptionPacket -> ClientError
    -- ^ Database responded with an exception packet
  InternalError :: HasCallStack => InternalError -> ClientError
  deriving anyclass (Exception)

instance Show ClientError where
  show (UnmatchedResult err) = "UserError " <> show err <> "\n" <> prettyCallStack callStack
  show (DatabaseException err) = "DatabaseException " <> show err <> "\n" <> prettyCallStack callStack
  show (InternalError err) = "InternalError " <> show err <> "\n" <> prettyCallStack callStack









-- * Deserialization

-- ** Generic API

type GenericClickHaskell record hasColumns =
  ( Generic record
  , GClickHaskell hasColumns (Rep record)
  )

class ClickHaskell columns record
  where
  default deserializeRecords :: GenericClickHaskell record columns => Bool -> ProtocolRevision -> UVarInt -> Get [record]
  deserializeRecords :: Bool -> ProtocolRevision -> UVarInt -> Get [record]
  deserializeRecords isCheckRequired rev size = (to <$!>) <$> gDeserializeRecords @columns isCheckRequired rev size

  default serializeRecords :: GenericClickHaskell record columns => [record] -> ProtocolRevision -> Builder
  serializeRecords :: [record] -> ProtocolRevision -> Builder
  serializeRecords records rev = gSerializeRecords @columns rev (from <$!> records)

  default columns :: GenericClickHaskell record columns => Builder
  columns :: Builder
  columns = buildCols (gReadingColumns @columns @(Rep record))
    where
    buildCols [] = mempty
    buildCols ((col, _):[])   = col
    buildCols ((col, _):rest) = col <> ", " <> buildCols rest

  default readingColumnsAndTypes :: GenericClickHaskell record columns => Builder
  readingColumnsAndTypes ::  Builder
  readingColumnsAndTypes = buildColsTypes (gReadingColumns @columns @(Rep record))
    where
    buildColsTypes [] = mempty
    buildColsTypes ((col, typ):[])   = col <> " " <> typ
    buildColsTypes ((col, typ):rest) = col <> " " <> typ <> ", " <> buildColsTypes rest

  default columnsCount :: GenericClickHaskell record columns => UVarInt
  columnsCount :: UVarInt
  columnsCount = gColumnsCount @columns @(Rep record)

class GClickHaskell (columns :: [Type]) f
  where
  gDeserializeRecords :: Bool -> ProtocolRevision -> UVarInt -> Get [f p]  
  gSerializeRecords :: ProtocolRevision -> [f p] -> Builder

  gReadingColumns :: [(Builder, Builder)]
  gColumnsCount :: UVarInt

instance
  GClickHaskell columns f
  =>
  GClickHaskell columns (D1 c (C1 c2 f))
  where
  {-# INLINE gDeserializeRecords #-}
  gDeserializeRecords isCheckRequired rev size = map (M1 . M1) <$> gDeserializeRecords @columns isCheckRequired rev size

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = gSerializeRecords @columns rev . map (unM1 . unM1)

  gReadingColumns = gReadingColumns @columns @f
  gColumnsCount = gColumnsCount @columns @f

instance
  (GClickHaskell columns left, GClickHaskell columns right)
  =>
  GClickHaskell columns (left :*: right)
  where
  {-# INLINE gDeserializeRecords #-}
  gDeserializeRecords isCheckRequired rev size = do
    liftA2 (zipWith (:*:))
      (gDeserializeRecords @columns @left isCheckRequired rev size)
      (gDeserializeRecords @columns @right isCheckRequired rev size)

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev xs =
    (\(ls,rs) -> gSerializeRecords @columns rev ls <> gSerializeRecords @columns rev rs)
      (foldr (\(l :*: r) (accL, accR) -> (l:accL, r:accR)) ([], []) xs)

  gReadingColumns = gReadingColumns @columns @left ++ gReadingColumns @columns @right
  gColumnsCount = gColumnsCount @columns @left + gColumnsCount @columns @right


instance
  ( KnownColumn (Column name chType)
  , SerializableColumn (Column name chType)
  , ToChType chType inputType
  , Column name chType ~ TakeColumn name columns
  ) => GClickHaskell columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  {-# INLINE gDeserializeRecords #-}
  gDeserializeRecords isCheckRequired rev size =
    either (throw . UnmatchedResult) (map (M1 . K1 . fromChType @chType) . columnValues)
      <$!> deserializeColumn @(Column name chType) rev isCheckRequired size

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev = serializeColumn rev . mkColumn @(Column name chType) . map (toChType . unK1 . unM1)

  gReadingColumns = (renderColumnName @(Column name chType), renderColumnType @(Column name chType)) : []
  gColumnsCount = 1


type family
  TakeColumn name columns :: Type
  where
  TakeColumn name columns = GoTakeColumn name columns '[]

type family
  GoTakeColumn name (columns :: [Type]) (acc :: [Type]) :: Type
  where
  GoTakeColumn name (Column name chType ': columns) acc = Column name chType
  GoTakeColumn name (Column name1 chType ': columns) acc = (GoTakeColumn name columns (Column name1 chType ': acc))
  GoTakeColumn name '[]                 acc = TypeError
    (    'Text "There is no column \"" :<>: 'Text name :<>: 'Text "\" in table"
    :$$: 'Text "You can't use this field"
    )


-- ** ToChType

class ToChType chType userType    where
  toChType   :: userType -> chType
  fromChType :: chType -> userType

instance {-# OVERLAPPABLE #-} (IsChType chType, chType ~ inputType) => ToChType chType inputType where
  toChType = id
  fromChType = id


instance ToChType ChString BS.ByteString where
  toChType = MkChString
  fromChType (MkChString string) = string

instance ToChType ChString Builder where
  toChType = MkChString . toStrict . toLazyByteString
  fromChType (MkChString string) = byteString string

instance ToChType ChString String where
  toChType = MkChString . BS8.pack
  fromChType (MkChString bs)= BS8.unpack bs


instance
  ToChType inputType chType
  =>
  ToChType (Nullable inputType) (Nullable chType)
  where
  toChType = fmap (toChType @inputType @chType)
  fromChType = fmap (fromChType @inputType)

instance
  ToChType inputType chType
  =>
  ToChType (LowCardinality inputType) chType where
  toChType = MkLowCardinality . toChType
  fromChType (MkLowCardinality lc)= fromChType @inputType lc

instance ToChType UUID (Word64, Word64) where
  toChType = MkUUID . uncurry (flip Word128)
  fromChType (MkUUID (Word128 w64hi w64lo)) = (w64hi, w64lo)

instance ToChType (DateTime tz) Word32     where
  toChType = MkDateTime
  fromChType (MkDateTime w32)= w32

instance ToChType (DateTime tz) UTCTime    where
  toChType = MkDateTime . floor . utcTimeToPOSIXSeconds
  fromChType (MkDateTime w32) = posixSecondsToUTCTime (fromIntegral w32)

instance ToChType (DateTime64 precision tz) Word64 where
  toChType = MkDateTime64
  fromChType (MkDateTime64 w64) = w64
instance ToChType Date Word16 where
  toChType = MkDate
  fromChType (MkDate w16) = w16

instance ToChType chType inputType => ToChType (Array chType) [inputType]
  where
  toChType = MkChArray . map toChType
  fromChType (MkChArray values) = map fromChType values
