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

  {- * Statements and commands -}

  {- ** Exceptions -}
  , ClientError(..)
  , ConnectionError(..)
  , UserError(..)
  , InternalError(..)

  {- ** Low level -}
  {- *** SELECT -}, select
  {- *** INSERT -}, insert
  {- *** Commands -}, command
  {- *** Ping -}, ping

  {- ** Wrappers -}
  , Table, View
  , selectFrom, selectFromView, generateRandom
  , insertInto

  {- ** Deriving -}
  , ClickHaskell(..)
  , ToChType(toChType, fromChType)
  , SerializableColumn
  , Column, KnownColumn

  {- ** Query -}
  , ToQueryPart(toQueryPart), parameter, Parameter, Parameters, viewParameters


  {- * ClickHouse types -}
  , IsChType(chTypeName, defaultValueOfTypeName)
  , DateTime(..), DateTime64
  , Int8, Int16, Int32, Int64, Int128(..)
  , UInt8, UInt16, UInt32, UInt64, UInt128, UInt256, Word128(..)
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
import Data.ByteString.Builder
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Generics (C1, D1, Generic (..), K1 (K1, unK1), M1 (M1, unM1), Meta (MetaSel), Rec0, S1, type (:*:) (..))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Prelude hiding (liftA2)

-- External
import Data.WideWord (Int128 (..), Word128 (..))
import Network.Socket hiding (SocketOption (..))

-- * Connection

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

-- * Statements and commands

-- ** Exceptions 

{- |
  A wrapper for all client-related errors

  You should this exception when you work with any ClickHaskell IO function.

  e.g. `command`, `select`, `insert` etc
  
-}
data ClientError where
  UnmatchedResult :: HasCallStack => UserError -> ClientError
    -- ^ Query result unmatched with declared specialization
  DatabaseException :: HasCallStack => ExceptionPacket -> ClientError
    -- ^ Database responded with an exception packet
  InternalError :: HasCallStack => InternalError -> ClientError
  deriving anyclass (Exception)

instance Show ClientError where
  show (UnmatchedResult err) = "UserError " <> show err <> "\n" <> prettyCallStack callStack
  show (DatabaseException err) = "DatabaseException " <> show err <> "\n" <> prettyCallStack callStack
  show (InternalError err) = "InternalError " <> show err <> "\n" <> prettyCallStack callStack


-- ** Low level

-- *** SELECT

select ::
  forall columns output result
  .
  ClickHaskell columns output
  =>
  Connection -> ChString -> ([output] -> IO result) -> IO [result]
select conn query f = do
  withConnection conn $ \connState -> do
    writeToConnection connState (serializeQueryPacket $ mkQueryArgs connState query)
    writeToConnection connState (serializeDataPacket "" 0 0)
    loopSelect connState []
  where
  loopSelect connState@MkConnectionState{..} acc =
    readBuffer buffer (deserialize revision)
    >>= \packet -> case packet of
      DataResponse MkDataPacket{columns_count = 0, rows_count = 0} -> loopSelect connState acc
      DataResponse MkDataPacket{columns_count, rows_count} -> do
        let expected = columnsCount @columns @output
        when (columns_count /= expected) $
          (throw . UnmatchedResult . UnmatchedColumnsCount)
            ("Expected " <> show expected <> " columns but got " <> show columns_count)
        !result <- f =<< readBuffer buffer (deserializeRecords @columns True revision rows_count)
        loopSelect connState (result : acc)
      Progress    _       -> loopSelect connState acc
      ProfileInfo _       -> loopSelect connState acc
      EndOfStream         -> pure acc
      Exception exception -> throwIO (DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)

-- *** INSERT

insert ::
  forall columns record
  .
  ClickHaskell columns record
  =>
  Connection -> ChString -> [record] -> IO ()
insert conn query columnsData = do
  withConnection conn $ \connState -> do
    writeToConnection connState (serializeQueryPacket $ mkQueryArgs connState query)
    writeToConnection connState (serializeDataPacket "" 0 0)
    loopInsert connState
  where
  loopInsert connState@MkConnectionState{..}  = do
    firstPacket <- readBuffer buffer (deserialize revision)
    case firstPacket of
      TableColumns      _ -> loopInsert connState 
      DataResponse MkDataPacket{} -> do
        _emptyDataPacket <- readBuffer buffer (deserializeRecords @columns @record False revision 0)
        let rows = fromIntegral (Prelude.length columnsData)
            cols = columnsCount @columns @record
        writeToConnection connState (serializeDataPacket "" cols rows)
        writeToConnection connState (serializeRecords @columns columnsData)
        writeToConnection connState (serializeDataPacket "" 0 0)
        loopInsert connState
      EndOfStream         -> pure ()
      Exception exception -> throwIO (DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)

-- *** Ping

{- |
  Sends `Ping` packet and handles `Pong` packet
-}
ping :: HasCallStack => Connection -> IO ()
ping conn = do
  withConnection conn $ \connState@MkConnectionState{revision, buffer} -> do
    writeToConnection connState (\rev -> serialize rev Ping)
    responsePacket <- readBuffer buffer (deserialize revision)
    case responsePacket of
      Pong                -> pure ()
      Exception exception -> throwIO (DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)

-- *** Commands

{- |
  Might be used for any command without data responses

  For example: CREATE, TRUNCATE, KILL, SET, GRANT

  __Throws exception if any data was returned__
-}
command :: HasCallStack => Connection -> ChString -> IO ()
command conn query = do
  withConnection conn $ \connState -> do
    writeToConnection connState (serializeQueryPacket (mkQueryArgs connState query))
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


-- ** Deriving

class ClickHaskell columns record
  where
  default deserializeRecords :: GenericClickHaskell record columns => Bool -> ProtocolRevision -> UVarInt -> Get [record]
  deserializeRecords :: Bool -> ProtocolRevision -> UVarInt -> Get [record]
  deserializeRecords isCheckRequired rev size =
    gDeserializeRecords @columns isCheckRequired rev size to

  default serializeRecords :: GenericClickHaskell record columns => [record] -> ProtocolRevision -> Builder
  serializeRecords :: [record] -> ProtocolRevision -> Builder
  serializeRecords records rev = gSerializeRecords @columns rev from records

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


-- ** Wrappers

type ClickHaskellTable table record =
  ( IsTable table
  , ClickHaskell (GetColumns table) record
  )

type ClickHaskellView view record =
  ( IsView view
  , ClickHaskell (GetColumns view) record
  )

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

insertInto ::
  forall table record
  .
  ClickHaskellTable table record
  =>
  Connection -> [record] -> IO ()
insertInto conn columnsData = insert @(GetColumns table) conn query columnsData
  where
  query = toChType $
    "INSERT INTO " <> tableName @table
    <> " (" <> columns @(GetColumns table) @record <> ") VALUES"




-- * Internal

mkQueryArgs :: ConnectionState -> ChString -> QueryPacketArgs
mkQueryArgs MkConnectionState{creds=MkConnectionArgs{..}} query
  = MkQueryPacketArgs{..}

-- ** Connection

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


-- ** Serialization Generic API

type GenericClickHaskell record hasColumns =
  ( Generic record
  , GClickHaskell hasColumns (Rep record)
  )

class GClickHaskell (columns :: [Type]) f
  where
  gDeserializeRecords :: Bool -> ProtocolRevision -> UVarInt -> (f p -> res) -> Get [res]
  gSerializeRecords :: ProtocolRevision -> (res -> f p) -> [res] -> Builder

  gReadingColumns :: [(Builder, Builder)]
  gColumnsCount :: UVarInt

instance
  GClickHaskell columns f
  =>
  GClickHaskell columns (D1 c (C1 c2 f))
  where
  {-# INLINE gDeserializeRecords #-}
  gDeserializeRecords isCheckRequired rev size f =
    gDeserializeRecords @columns isCheckRequired rev size (f . M1 . M1)

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev f = gSerializeRecords @columns rev (unM1 . unM1 . f)

  gReadingColumns = gReadingColumns @columns @f
  gColumnsCount = gColumnsCount @columns @f

instance
  (GClickHaskell columns left, GClickHaskell columns right)
  =>
  GClickHaskell columns (left :*: right)
  where
  {-# INLINE gDeserializeRecords #-}
  gDeserializeRecords isCheckRequired rev size f = do
    liftA2 (\l r -> f <$!> zipWith (:*:) l r)
      (gDeserializeRecords @columns @left isCheckRequired rev size id)
      (gDeserializeRecords @columns @right isCheckRequired rev size id)

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev f xs  =
    (\(ls,rs) -> gSerializeRecords @columns rev id ls  <> gSerializeRecords @columns rev id rs)
      (foldr (\(l :*: r) (accL, accR) -> (l:accL, r:accR)) ([], []) (map f xs))

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
  gDeserializeRecords isCheckRequired rev size f = do
    handleColumnHeader @(Column name chType) isCheckRequired rev
    map (f . M1 . K1 . fromChType) . columnValues
      <$!> deserializeColumn @(Column name chType) rev size

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev f values
    =  serialize @ChString rev (toChType (renderColumnName @(Column name chType)))
    <> serialize @ChString rev (toChType (renderColumnType @(Column name chType)))
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @UInt8 rev 0)
    <> (serializeColumn rev . mkColumn @(Column name chType) . map (toChType . unK1 . unM1 . f)) values

  gReadingColumns = (renderColumnName @(Column name chType), renderColumnType @(Column name chType)) : []
  gColumnsCount = 1

handleColumnHeader :: forall column . KnownColumn column => Bool -> ProtocolRevision -> Get ()
handleColumnHeader isCheckRequired rev = do
  let expectedColumnName = toChType (renderColumnName @column)
  resultColumnName <- deserialize @ChString rev
  when (isCheckRequired && resultColumnName /= expectedColumnName) $
    throw . UnmatchedResult . UnmatchedColumn
      $ "Got column \"" <> show resultColumnName <> "\" but expected \"" <> show expectedColumnName <> "\""

  let expectedType = toChType (renderColumnType @column)
  resultType <- deserialize @ChString rev
  when (isCheckRequired && resultType /= expectedType) $
    throw . UnmatchedResult . UnmatchedType
      $ "Column " <> show resultColumnName <> " has type " <> show resultType <> ". But expected type is " <> show expectedType

  _isCustom <- deserialize @(UInt8 `SinceRevision` DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION) rev
  pure ()

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
