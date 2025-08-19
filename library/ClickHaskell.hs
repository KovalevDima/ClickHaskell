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

  {- ** SELECT -}
  {- *** Runner -}, select, selectCols
  {- *** Statements -}
  , Select, unsafeMkSelect
  , fromGenerateRandom
  , fromTable
  {- *** View -}
  , fromView
  , parameter, Parameter, Parameters, viewParameters
  {- ** INSERT -}
  , Insert, unsafeMkInsert
  , insert, insertCols
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
import Control.Concurrent (newMVar, putMVar, takeMVar)
import Control.Exception (Exception, SomeException, bracketOnError, catch, finally, mask, onException, throw, throwIO)
import Control.Monad (when)
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
  ClickHaskell columns output
  =>
  Select columns output -> Connection -> ([output] -> IO result) -> IO [result]
select (MkSelect mkQuery) conn f = do
  withConnection conn $ \connState -> do
    writeToConnection connState
      (serializeQueryPacket . mkQueryArgs connState . mkQuery $ expectedColumns @columns @output)
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

{- |
  Takes `Select`, `Connection` and __block processing__ function

  Returns __block processing__ result
-}
selectCols ::
  forall columns output result
  .
  ClickHaskell columns output
  =>
  Select columns output -> Connection -> (Columns (ExpectedColumns columns output) -> IO result) -> IO [result]
selectCols (MkSelect mkQuery) conn f = do
  withConnection conn $ \connState -> do
    writeToConnection connState
      (serializeQueryPacket . mkQueryArgs connState . mkQuery $ expectedColumns @columns @output)
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
        !result <- f =<< readBuffer buffer (deserializeColumns @columns @output True revision rows_count)
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
  Insert columns record -> Connection -> [record] -> IO ()
insert (MkInsert mkQuery) conn columnsData = do
  withConnection conn $ \connState -> do
    writeToConnection connState
      . serializeQueryPacket
      . mkQueryArgs connState
      . mkQuery
      $ expectedColumns @columns @record
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

insertCols ::
  forall columns record
  .
  ClickHaskell columns record
  =>
  Insert columns record -> Connection -> Columns (ExpectedColumns columns record) -> IO ()
insertCols (MkInsert mkQuery) conn columnsData = do
  withConnection conn $ \connState -> do
    writeToConnection connState
      . serializeQueryPacket
      . mkQueryArgs connState
      . mkQuery
      $ expectedColumns @columns @record
    writeToConnection connState (serializeDataPacket "" 0 0)
    loopInsert connState
  where
  loopInsert connState@MkConnectionState{..}  = do
    firstPacket <- readBuffer buffer (deserialize revision)
    case firstPacket of
      TableColumns      _ -> loopInsert connState
      DataResponse MkDataPacket{} -> do
        _emptyDataPacket <- readBuffer buffer (deserializeRecords @columns @record False revision 0)
        let rows = fromIntegral (colLen columnsData)
            cols = columnsCount @columns @record
        writeToConnection connState (serializeDataPacket "" cols rows)
        writeToConnection connState (serializeColumns @columns @record columnsData)
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
  deserializeRecords doCheck rev size = gDeserializeRecords @columns doCheck rev size to

  default serializeRecords :: GenericClickHaskell record columns => [record] -> ProtocolRevision -> Builder
  serializeRecords :: [record] -> ProtocolRevision -> Builder
  serializeRecords records rev = gSerializeRecords @columns rev records from

  default expectedColumns :: GenericClickHaskell record columns => [(Builder, Builder)]
  expectedColumns :: [(Builder, Builder)]
  expectedColumns = gReadingColumns @columns @(Rep record)

  default columnsCount :: GenericClickHaskell record columns => UVarInt
  columnsCount :: UVarInt
  columnsCount = gColumnsCount @columns @(Rep record)

  default deserializeColumns :: GenericClickHaskell record columns => Bool -> ProtocolRevision -> UVarInt -> Get (Columns (ExpectedColumns columns record))
  deserializeColumns :: Bool -> ProtocolRevision -> UVarInt -> Get (Columns (ExpectedColumns columns (record)))
  deserializeColumns doCheck rev size = gDeserializeColumns @columns @(Rep record) doCheck rev size

  default serializeColumns :: GenericClickHaskell record columns => Columns (GExpectedColumns columns (Rep record)) -> ProtocolRevision -> Builder
  serializeColumns :: Columns (GExpectedColumns columns (Rep record)) -> ProtocolRevision -> Builder
  serializeColumns columns rev = gSerializeColumns @columns @(Rep record) rev columns

type GenericClickHaskell record hasColumns =
  ( Generic record
  , GClickHaskell hasColumns (Rep record)
  )


type family ExpectedColumns columns record :: [Type]
  where
  ExpectedColumns columns record = GExpectedColumns columns (Rep record)



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


class GClickHaskell (columns :: [Type]) f
  where
  {-
    Generic deriving can be a bit tricky

    You can think of it as
    1) Columns serialization logic generator
    2) Columns-to-rows(list of records) transposer
  -}
  gDeserializeRecords :: Bool -> ProtocolRevision -> UVarInt -> (f p -> res) -> Get [res]
  gSerializeRecords :: ProtocolRevision -> [res] -> (res -> f p) -> Builder
  {-
    and affected columns extractor
  -}
  gReadingColumns :: [(Builder, Builder)]
  gColumnsCount :: UVarInt

  type GExpectedColumns columns f :: [Type]
  gDeserializeColumns :: Bool -> ProtocolRevision -> UVarInt -> Get (Columns (GExpectedColumns columns f))
  gSerializeColumns :: ProtocolRevision -> Columns (GExpectedColumns columns f) -> Builder

{-
  Unwrapping data type constructor
    data Record = MkRecord ..
-}
instance
  GClickHaskell columns f
  =>
  GClickHaskell columns (D1 c (C1 c2 f))
  where
  {-# INLINE gDeserializeRecords #-}
  gDeserializeRecords doCheck rev size f = gDeserializeRecords @columns doCheck rev size (f . M1 . M1)

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev xs f = gSerializeRecords @columns rev xs (unM1 . unM1 . f)

  gReadingColumns = gReadingColumns @columns @f
  gColumnsCount = gColumnsCount @columns @f

  type GExpectedColumns columns (D1 c (C1 c2 f)) = GExpectedColumns columns f
  gDeserializeColumns doCheck rev size = gDeserializeColumns @columns @f doCheck rev size
  {-# INLINE gDeserializeColumns #-}
  gSerializeColumns rev col = gSerializeColumns @columns @f rev col
  {-# INLINE gSerializeColumns #-}

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
  {-# INLINE gDeserializeRecords #-}
  gDeserializeRecords doCheck rev size f =
    gDeserializeRecords @columns @(left :*: (right1 :*: right2)) doCheck rev size
      (\(l :*: (r1:*:r2)) -> f ((l :*: r1):*:r2))

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev xs f =
    gSerializeRecords @columns @(left :*: (right1 :*: right2)) rev xs
      ((\((l:*:r1) :*: r2) -> l :*: (r1 :*: r2)) . f)

  gReadingColumns = gReadingColumns @columns @(left :*: (right1 :*: right2))
  gColumnsCount = gColumnsCount @columns @(left :*: (right1 :*: right2))

  type GExpectedColumns columns ((left :*: right1) :*: right2) = GExpectedColumns columns (left :*: (right1 :*: right2))
  gDeserializeColumns doCheck rev size = gDeserializeColumns @columns @(left :*: (right1 :*: right2)) doCheck rev size
  {-# INLINE gDeserializeColumns #-}
  gSerializeColumns rev col = gSerializeColumns @columns @(left :*: (right1 :*: right2)) rev col
  {-# INLINE gSerializeColumns #-}

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
  {-# INLINE gDeserializeRecords #-}
  gDeserializeRecords doCheck rev size f = do
    lefts  <- gDeserializeRecords @columns @(S1 (MetaSel (Just name) a b f) (Rec0 inputType)) doCheck rev size id
    rights <- gDeserializeRecords @columns @right doCheck rev size id
    deserializeProduct (\l r -> f $ l :*: r) lefts rights

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev xs f
    =  gSerializeRecords @columns rev xs ((\(l:*:_) -> l) . f)
    <> gSerializeRecords @columns rev xs ((\(_:*:r) -> r) . f)

  gReadingColumns = gReadingColumns @columns @(S1 (MetaSel (Just name) a b f) (Rec0 inputType)) ++ gReadingColumns @columns @right
  gColumnsCount = gColumnsCount @columns @(S1 (MetaSel (Just name) a b f) (Rec0 inputType)) + gColumnsCount @columns @right

  type GExpectedColumns columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType) :*: right)
    = Column name (GetColumnType (TakeColumn name columns)) ': GExpectedColumns columns right
  gDeserializeColumns doCheck rev size = do
    (AddColumn col Empty) <- gDeserializeColumns @columns @(S1 (MetaSel (Just name) a b f) (Rec0 inputType)) doCheck rev size
    rights <- gDeserializeColumns @columns @right doCheck rev size
    pure $ AddColumn col rights
  {-# INLINE gDeserializeColumns #-}
  gSerializeColumns rev (AddColumn col cols) =
    gSerializeColumns @(columns) @(S1 (MetaSel (Just name) a b f) (Rec0 inputType)) rev (AddColumn col Empty)
    <> gSerializeColumns @columns @right rev cols
  {-# INLINE gSerializeColumns #-}

deserializeProduct ::  (l -> r -> a) -> [l] -> [r] -> Get [a]
deserializeProduct f lefts rights = goDeserialize [] lefts rights
  where
  goDeserialize !acc (l:ls) (r:rs) = goDeserialize ((:acc) $! f l r) ls rs
  goDeserialize !acc [] [] = pure acc
  goDeserialize _ _ _ = fail "Mismatched lengths in gDeserializeRecords"

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
  {-# INLINE gDeserializeRecords #-}
  gDeserializeRecords doCheck rev size f = do
    handleColumnHeader @(Column name chType) doCheck rev
    deserializeColumn @(Column name chType) rev size (f . M1 . K1 . fromChType)

  {-# INLINE gSerializeRecords #-}
  gSerializeRecords rev values f
    =  serialize @ChString rev (toChType (renderColumnName @(Column name chType)))
    <> serialize @ChString rev (toChType (renderColumnType @(Column name chType)))
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @UInt8 rev 0)
    <> serializeColumn @(Column name chType) rev (toChType . unK1 . unM1 . f) values

  gReadingColumns = (renderColumnName @(Column name chType), renderColumnType @(Column name chType)) : []
  gColumnsCount = 1

  type GExpectedColumns columns ((S1 (MetaSel (Just name) a b f)) (Rec0 inputType))
    = '[Column name (GetColumnType (TakeColumn name columns))]
  gDeserializeColumns doCheck rev size = do
    handleColumnHeader @(Column name chType) doCheck rev
    col <- deserializeColumn @(Column name chType) rev size id
    pure $ AddColumn (toColumn @(Column name chType) col) Empty
  {-# INLINE gDeserializeColumns #-}
  gSerializeColumns rev (AddColumn col Empty)
    =  serialize @ChString rev (toChType (renderColumnName @(Column name chType)))
    <> serialize @ChString rev (toChType (renderColumnType @(Column name chType)))
    <> afterRevision @DBMS_MIN_REVISION_WITH_CUSTOM_SERIALIZATION rev (serialize @UInt8 rev 0)
    <> serializeColumn @(Column name chType) rev id (fromColumn @(Column name chType) col)
  {-# INLINE gSerializeColumns #-}

handleColumnHeader :: forall column . KnownColumn column => Bool -> ProtocolRevision -> Get ()
handleColumnHeader doCheck rev = do
  let expectedColumnName = toChType (renderColumnName @column)
  resultColumnName <- deserialize @ChString rev
  when (doCheck && resultColumnName /= expectedColumnName) $
    throw . UnmatchedResult . UnmatchedColumn
      $ "Got column \"" <> show resultColumnName <> "\" but expected \"" <> show expectedColumnName <> "\""

  let expectedType = toChType (renderColumnType @column)
  resultType <- deserialize @ChString rev
  when (doCheck && resultType /= expectedType) $
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
