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
  , Connection(..), openConnection
  {- ** Hacking  -}
  , overrideInitConnection
  , overrideHostname
  , overrideOsUser
  , overrideDefaultPort
  , overrideMaxRevision
  , mkBuffer

  {- * Statements and commands -}

  {- ** Exceptions -}
  , ClientError(..)
  , ConnectionError(..)
  , UserError(..)
  , InternalError(..)

  {- ** Settings -}
  , passSettings
  , addSetting

  {- ** SELECT -}
  {- *** Runner -}, select
  {- *** Statements -}
  , Select, unsafeMkSelect
  , fromGenerateRandom
  , fromTable
  {- *** View -}
  , fromView
  , parameter, Parameter, Parameters, viewParameters

  {- ** INSERT -}
  , Insert, unsafeMkInsert
  , insert
  , intoTable
  {- *** Modifiers -}
  , ToQueryPart(toQueryPart)
  
  {- ** Ping -}, ping
  {- ** Commands -}, command

  {- ** Deriving -}
  , ClickHaskell(..)
  , ToChType(toChType, fromChType)
  , SerializableColumn
  , Column, KnownColumn


  {- * ClickHouse types -}
  , IsChType(chTypeName, defaultValueOfTypeName)
  , DateTime, DateTime64
  , Int8, Int16, Int32, Int64, Int128(..)
  , UInt8, UInt16, UInt32, UInt64, UInt128, UInt256, Word128(..)
  , Nullable
  , LowCardinality, IsLowCardinalitySupported
  , UUID
  , Array
  , ChString
  , Enum8, Enum16
  ) where

-- Internal
import ClickHaskell.Columns
import ClickHaskell.Connection
import ClickHaskell.Primitive
import ClickHaskell.Statements
import ClickHaskell.Packets.Client
import ClickHaskell.Packets.Data
import ClickHaskell.Packets.Server
import ClickHaskell.Packets.Settings

-- GHC included
import Control.Concurrent (newMVar, putMVar, takeMVar)
import Control.Exception (Exception, mask, onException, throw, throwIO)
import Control.Monad (when)
import Data.Binary.Get
import Data.ByteString.Builder
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import GHC.Generics (C1, D1, Generic (..), K1 (K1, unK1), M1 (M1, unM1), Meta (MetaSel), Rec0, S1, type (:*:) (..))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import System.Environment (lookupEnv)

-- External
import Data.WideWord (Int128 (..), Word128 (..))

-- * Connection

openConnection :: HasCallStack => ConnectionArgs -> IO Connection
openConnection creds@MkConnectionArgs{mHostname, mOsUser} = do
  hostname <- maybe (lookupEnv "HOSTNAME") (pure . Just) mHostname
  osUser   <- maybe (lookupEnv "USER")     (pure . Just) mOsUser
  connectionState <-
    createConnectionState auth
      . (maybe id overrideHostname hostname)
      . (maybe id overrideOsUser osUser)
      $ creds
  MkConnection <$> newMVar connectionState

-- * Statements and commands

-- ** Exceptions 

{- |
  A wrapper for all client-related errors

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

{- |
  Errors intended to be handled by developers
-}
data UserError
  = UnmatchedType String
  -- ^ Column type mismatch in data packet
  | UnmatchedColumn String
  -- ^ Column name mismatch in data packet
  | UnmatchedColumnsCount String
  -- ^ Occurs when actual columns count less or more than expected
  deriving (Show, Exception)


-- ** Low level

-- *** SELECT

{- |
  Takes `Select`, `Connection` and __block processing__ function

  Returns __block processing__ result
-}
select ::
  forall columns output result
  .
  HasCallStack
  =>
  ClickHaskell columns output
  =>
  Select columns output -> Connection -> ([output] -> IO result) -> IO [result]
select (MkSelect mkQuery setts) conn f = do
  withConnection conn $ \connState -> do
    writeToConnection connState
      . serializeQueryPacket
      . mkQueryArgs connState setts
      . mkQuery
      $ expectedColumns @columns @output
    writeToConnection connState (\rev -> serialize rev . Data $ mkDataPacket "" 0 0)
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
        !result <- f =<< readBuffer buffer (deserializeColumns @columns True revision rows_count)
        loopSelect connState (result : acc)
      ProfileEvents _     -> loopSelect connState acc
      Progress    _       -> loopSelect connState acc
      ProfileInfo _       -> loopSelect connState acc
      EndOfStream         -> pure acc
      Exception exception -> throwIO (DatabaseException exception)
      otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


-- *** INSERT

insert ::
  forall columns record
  .
  HasCallStack
  =>
  ClickHaskell columns record
  =>
  Insert columns record -> Connection -> [record] -> IO ()
insert (MkInsert mkQuery dbSettings) conn columnsData = do
  withConnection conn $ \connState -> do
    writeToConnection connState
      . serializeQueryPacket
      . mkQueryArgs connState dbSettings
      . mkQuery
      $ expectedColumns @columns @record
    writeToConnection connState (\rev -> serialize rev . Data $ mkDataPacket "" 0 0)
    loopInsert connState
  where
  loopInsert connState@MkConnectionState{..} = do
    firstPacket <- readBuffer buffer (deserialize revision)
    case firstPacket of
      TableColumns      _ -> loopInsert connState 
      DataResponse MkDataPacket{} -> do
        _emptyDataPacket <- readBuffer buffer (deserializeColumns @columns @record False revision 0)
        let rows = fromIntegral (Prelude.length columnsData)
            cols = columnsCount @columns @record
        writeToConnection connState (\rev -> serialize rev . Data $ mkDataPacket "" cols rows)
        writeToConnection connState (serializeColumns @columns columnsData)
        writeToConnection connState (\rev -> serialize rev . Data $ mkDataPacket "" 0 0)
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
    writeToConnection connState (serializeQueryPacket (mkQueryArgs connState (MkDbSettings []) query))
    writeToConnection connState (\rev -> serialize rev . Data $ mkDataPacket "" 0 0)
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
  default deserializeColumns :: GenericClickHaskell record columns => Bool -> ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns :: Bool -> ProtocolRevision -> UVarInt -> Get [record]
  deserializeColumns doCheck rev size = gDeserializeColumns @columns doCheck rev size to

  default serializeColumns :: GenericClickHaskell record columns => [record] -> ProtocolRevision -> Builder
  serializeColumns :: [record] -> ProtocolRevision -> Builder
  serializeColumns records rev = gSerializeRecords @columns rev records from

  default expectedColumns :: GenericClickHaskell record columns => [(Builder, Builder)]
  expectedColumns :: [(Builder, Builder)]
  expectedColumns = gExpectedColumns @columns @(Rep record)

  default columnsCount :: GenericClickHaskell record columns => UVarInt
  columnsCount :: UVarInt
  columnsCount = gColumnsCount @columns @(Rep record)

type GenericClickHaskell record hasColumns =
  ( Generic record
  , GClickHaskell hasColumns (Rep record)
  )




-- * Internal

mkQueryArgs :: ConnectionState -> DbSettings -> ChString -> QueryPacketArgs
mkQueryArgs MkConnectionState {..} settings query = MkQueryPacketArgs {..}

-- ** Connection

readBuffer :: Buffer -> Get a -> IO a
readBuffer MkBuffer{readBuff, writeBuff} parser = runBufferReader (runGetIncremental parser)
  where
  runBufferReader :: Decoder packet -> IO packet
  runBufferReader = \case
    (Partial decoder) -> readBuff >>= runBufferReader . decoder . Just
    (Done leftover _consumed packet) -> packet <$ writeBuff leftover
    (Fail _leftover _consumed msg) -> throwIO  (InternalError $ DeserializationError msg)


withConnection :: HasCallStack => Connection -> (ConnectionState -> IO a) -> IO a
withConnection (MkConnection connStateMVar) f =
  mask $ \restore -> do
    connState <- takeMVar connStateMVar
    b <- onException
      (restore (f connState))
      (do
        newConnState <- recreateConnectionState auth connState
        putMVar connStateMVar newConnState
      )
    putMVar connStateMVar connState
    return b

auth :: Buffer -> ConnectionArgs -> IO ConnectionState
auth buffer creds@MkConnectionArgs{db, user, pass, mOsUser, mHostname, maxRevision} = do
  (writeConn buffer . seriliazeHelloPacket db user pass) maxRevision
  serverPacketType <- readBuffer buffer (deserialize maxRevision)
  case serverPacketType of
    HelloResponse MkHelloResponse{server_revision} -> do
      let conn =
            MkConnectionState
              { revision     = min server_revision maxRevision
              , os_user      = maybe "" toChType mOsUser
              , hostname     = maybe "" toChType mHostname
              , initial_user = toChType user
              , ..
              }
      writeToConnection conn (\rev -> serialize rev mkAddendum)
      pure conn
    Exception exception -> throwIO (DatabaseException exception)
    otherPacket         -> throwIO (InternalError $ UnexpectedPacketType $ serverPacketToNum otherPacket)


-- ** Serialization Generic API

class GClickHaskell (columns :: [Type]) f
  where

  {-
    Generic deriving can be a bit tricky

    You can think of it as
    1) Columns serialization logic generator
    2) Columns-to-rows(list of records) transposer
  -}
  gDeserializeColumns :: Bool -> ProtocolRevision -> UVarInt -> (f p -> res) -> Get [res]
  gSerializeRecords :: ProtocolRevision -> [res] -> (res -> f p) -> Builder
  {-
    and affected columns extractor
  -}
  gExpectedColumns :: [(Builder, Builder)]
  gColumnsCount :: UVarInt

{-
  Unwrapping data type constructor
    data Record = MkRecord ..
-}
instance
  GClickHaskell columns f
  =>
  GClickHaskell columns (D1 c (C1 c2 f))
  where
  {-# INLINE gDeserializeColumns #-}
  gDeserializeColumns doCheck rev size f =
    gDeserializeColumns @columns doCheck rev size (f . M1 . M1)

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev xs f = gSerializeRecords @columns rev xs (unM1 . unM1 . f)

  gExpectedColumns = gExpectedColumns @columns @f
  gColumnsCount = gColumnsCount @columns @f

{-
  Flattening of generic products

  For example
    (
      field_1::T1 :*: field_2::T2)
    ) :*: (
        field_3::T3 :*: field_4::T4
      )

  turns into
    field_1::T1 :*: (
      field_2::T2 :*: (field_3::T3 :*: field_4::T4)
    )
-}
instance
  GClickHaskell columns (left :*: (right1 :*: right2))
  =>
  GClickHaskell columns ((left :*: right1) :*: right2)
  where
  {-# INLINE gDeserializeColumns #-}
  gDeserializeColumns doCheck rev size f =
    gDeserializeColumns @columns @(left :*: (right1 :*: right2)) doCheck rev size
      (\(l :*: (r1:*:r2)) -> f ((l :*: r1):*:r2))

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev xs f =
    gSerializeRecords @columns @(left :*: (right1 :*: right2)) rev xs
      ((\((l:*:r1) :*: r2) -> l :*: (r1 :*: r2)) . f)

  gExpectedColumns = gExpectedColumns @columns @(left :*: (right1 :*: right2))
  gColumnsCount = gColumnsCount @columns @(left :*: (right1 :*: right2))

{-
  Unwrapping a product starting with a field

  field_n::Tn :*: (..)
-}
instance
  ( GClickHaskell columns right
  , KnownColumn (Column name chType)
  , SerializableColumn (Column name chType)
  , ToChType chType inputType
  , Column name chType ~ TakeColumn name columns
  )
  =>
  GClickHaskell columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType) :*: right)
  where
  {-# INLINE gDeserializeColumns #-}
  gDeserializeColumns doCheck rev size f = do
    lefts  <- gDeserializeColumns @columns @(S1 (MetaSel (Just name) a b f) (Rec0 inputType)) doCheck rev size id
    rights <- gDeserializeColumns @columns @right doCheck rev size id
    deserializeProduct (\l r -> f $ l :*: r) lefts rights

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev xs f
    =  gSerializeRecords @columns rev xs ((\(l:*:_) -> l) . f)
    <> gSerializeRecords @columns rev xs ((\(_:*:r) -> r) . f)

  gExpectedColumns = gExpectedColumns @columns @(S1 (MetaSel (Just name) a b f) (Rec0 inputType)) ++ gExpectedColumns @columns @right
  gColumnsCount = gColumnsCount @columns @(S1 (MetaSel (Just name) a b f) (Rec0 inputType)) + gColumnsCount @columns @right

deserializeProduct ::  (l -> r -> a) -> [l] -> [r] -> Get [a]
deserializeProduct f lefts rights = goDeserialize [] lefts rights
  where
  goDeserialize !acc (l:ls) (r:rs) = goDeserialize ((:acc) $! f l r) ls rs
  goDeserialize !acc [] [] = pure acc
  goDeserialize _ _ _ = fail "Mismatched lengths in gDeserializeColumns"

{-
  Unwrapping a single generic field (recursion breaker)

  field::Tn
-}
instance
  ( KnownColumn (Column name chType)
  , SerializableColumn (Column name chType)
  , ToChType chType inputType
  , Column name chType ~ TakeColumn name columns
  ) => GClickHaskell columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
  where
  {-# INLINE gDeserializeColumns #-}
  gDeserializeColumns doCheck rev size f = do
    validateColumnHeader @(Column name chType) doCheck =<< deserialize @ColumnHeader rev
    deserializeColumn @(Column name chType) rev size (f . M1 . K1 . fromChType)

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev values f
    =  serialize rev (mkHeader @(Column name chType))
    <> serializeColumn @(Column name chType) rev (toChType . unK1 . unM1 . f) values

  gExpectedColumns = (renderColumnName @(Column name chType), renderColumnType @(Column name chType)) : []
  gColumnsCount = 1

validateColumnHeader :: forall column . KnownColumn column => Bool -> ColumnHeader -> Get ()
validateColumnHeader doCheck MkColumnHeader{..} = do
  let expectedColumnName = toChType (renderColumnName @column)
      resultColumnName = name
  when (doCheck && resultColumnName /= expectedColumnName) $
    throw . UnmatchedResult . UnmatchedColumn
      $ "Got column \"" <> show resultColumnName <> "\" but expected \"" <> show expectedColumnName <> "\""

  let expectedType = toChType (renderColumnType @column)
      resultType = type_
  when (doCheck && resultType /= expectedType) $
    throw . UnmatchedResult . UnmatchedType
      $ "Column " <> show resultColumnName <> " has type " <> show resultType <> ". But expected type is " <> show expectedType

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
