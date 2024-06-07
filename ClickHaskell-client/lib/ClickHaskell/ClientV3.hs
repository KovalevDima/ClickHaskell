{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , OverloadedStrings
  , TypeFamilyDependencies
  , ConstraintKinds
#-}
module ClickHaskell.ClientV3 where

-- Internal
import ClickHaskell.Generics (WritableInto(..), ReadableFrom(..))
import ClickHaskell.Tables   (InterpretableTable(..), Table, renderTable, View, renderView)


-- External
import Network.HTTP.Types          as H (Status(..))
import Network.HTTP.Client         as H
  ( httpLbs, newManager
  , Manager, ManagerSettings(..)
  , Request(..), Response(..), RequestBody(..)
  )
import Network.HTTP.Client.Conduit as H
  (defaultManagerSettings, parseRequest, responseTimeoutNone)


-- GHC included
import Control.DeepSeq            (NFData)
import Control.Exception          (throw, Exception)
import Control.Monad              (when, void)
import Data.ByteString            as BS (toStrict, empty)
import Data.ByteString.Builder    (toLazyByteString)
import Data.ByteString.Lazy       as BL (LazyByteString, toChunks)
import Data.ByteString.Lazy.Char8 as BSL8 (lines)
import Data.IORef                 (newIORef, readIORef, writeIORef)
import Data.Kind                  (Type)
import Data.Proxy                 (Proxy(..))
import Data.Text                  as T (Text, unpack)
import Data.Text.Encoding         as T (encodeUtf8, decodeUtf8)
import GHC.Generics               (Generic)



-- * Client language parts

class InterpretExpression (expression :: Expression Type Type) where
  type InterpretRequest expression :: Type
  type InterpretResponse expression :: Type
  toRequest :: InterpretRequest expression
  fromResponse :: ChResponse expression -> InterpretResponse expression

data Expression table record
  = ReadTable table record
  | ReadView table record
  | WriteTable table record
  | Raw

newtype ChRequest (expression :: Expression Type Type)
  = MkChRequest { requestBody :: BL.LazyByteString }
  deriving (Show)

newtype ChResponse (expression :: Expression Type Type)
  = MkChResponse { responseBody :: BL.LazyByteString }
  deriving (Show)


-- ** Reading Table

instance
  ( ReadableFrom (Table name columns) record
  , InterpretableTable (Table name columns)
  ) =>
  InterpretExpression ('ReadTable (Table name columns) record)
  where
  type InterpretRequest ('ReadTable (Table name columns) record)
    = ChRequest ('ReadTable (Table name columns) record)
  type InterpretResponse ('ReadTable (Table name columns) record) = [record]

  toRequest = MkChRequest . toLazyByteString
    $  "SELECT " <> readingColumns @(Table name columns) @record
    <> " FROM " <> renderTable (interpretTable @(Table name columns))

  fromResponse (MkChResponse body) = map
    (fromTsvLine @(Table name columns) @record . BS.toStrict)
    $ BSL8.lines body

type ReadTableInterpretation table record =
  ( ReadableFrom table record
  , InterpretableTable table
  , InterpretExpression ('ReadTable table record)
  , InterpretRequest ('ReadTable table record) ~ ChRequest ('ReadTable table record)
  , InterpretResponse ('ReadTable table record) ~ [record]
  )

readTableExpression :: forall table record
  .  ReadTableInterpretation table record
  => Proxy table
  -> Proxy record
  -> ChRequest ('ReadTable table record)
readTableExpression _ _ = toRequest @('ReadTable table record)

runReadTable' :: forall table record client
  .  (ReadTableInterpretation table record, ChClient client)
  => client
  -> Proxy table
  -> Proxy record
  -> (ChResponse ('ReadTable table record) -> IO ())
  -> IO [record]
runReadTable' client pTable pRecord action  = do
  response <- runClient client $ readTableExpression pTable pRecord
  action response
  pure $ fromResponse response

runReadTable ::
  forall table record client
  . (ReadTableInterpretation table record, ChClient client)
  => client
  -> Proxy table
  -> Proxy record
  -> IO [record]
runReadTable client table record = runReadTable' client table record
  . const $ pure ()


-- ** Reading View

instance
  ( ReadableFrom (View name columns params) record
  , InterpretableTable (View name columns params)
  ) =>
  InterpretExpression ('ReadView (View name columns params) record)
  where
  type InterpretRequest ('ReadView (View name columns params) record)
    =  View name columns '[]
    -> ChRequest ('ReadView (View name columns params) record)
  type InterpretResponse ('ReadView (View name columns params) record) = [record]

  toRequest view = MkChRequest . toLazyByteString
    $  "SELECT " <> readingColumns @(View name columns params) @record
    <> " FROM " <> renderView view

  fromResponse (MkChResponse body) = map
    (fromTsvLine @(View name columns params) @record . BS.toStrict)
    $ BSL8.lines body

type ReadViewInterpretation view record name columns =
  ( ReadableFrom view record
  , InterpretableTable view
  , InterpretExpression ('ReadView view record)
  , InterpretRequest ('ReadView view record) ~ (View name columns '[] -> ChRequest ('ReadView view record))
  , InterpretResponse ('ReadView view record) ~ [record]
  )

readViewExpression :: forall view record name columns
  .  ReadViewInterpretation view record name columns
  => Proxy view
  -> Proxy record
  -> View name columns '[]
  -> ChRequest ('ReadView view record)
readViewExpression _ _ = toRequest @('ReadView view record)

runReadView' :: forall view record name columns client
  .  (ReadViewInterpretation view record name columns, ChClient client)
  => client
  -> Proxy view
  -> Proxy record
  -> View name columns '[]
  -> (ChResponse ('ReadView view record) -> IO ())
  -> IO [record]
runReadView' client pView pRecord view action = do
  response <- runClient client $ readViewExpression pView pRecord view
  action response
  pure $ fromResponse response

runReadView :: forall view record name columns client
  .  (ReadViewInterpretation view record name columns, ChClient client)
  => client
  -> Proxy view
  -> Proxy record
  -> View name columns '[]
  -> IO [record]
runReadView client pView pRecord view = runReadView' client pView pRecord view
  . const $ pure ()


-- ** Writing

instance
  ( WritableInto (Table name columns) record
  , InterpretableTable (Table name columns)
  ) =>
  InterpretExpression ('WriteTable (Table name columns) record)
  where
  type InterpretRequest ('WriteTable (Table name columns) record)
    =  [record]
    -> ChRequest ('WriteTable (Table name columns) record)
  type InterpretResponse ('WriteTable (Table name columns) record) = ()

  toRequest records = MkChRequest . toLazyByteString
    $  "INSERT INTO " <> renderTable (interpretTable @(Table name columns))
    <> " (" <> writingColumns @(Table name columns) @record <> ")"
    <> " FORMAT TSV\n"
    <> mconcat (map (toTsvLine @(Table name columns)) records)

  fromResponse = const ()

type WriteTableInterpretation table record =
  ( WritableInto table record
  , InterpretExpression ('WriteTable table record)
  , InterpretRequest ('WriteTable table record) ~ ([record] -> ChRequest ('WriteTable table record))
  , InterpretResponse ('WriteTable table record) ~ ()
  )

writeExpression :: forall table record
  .  WriteTableInterpretation table record
  => Proxy table
  -> [record]
  -> ChRequest ('WriteTable table record)
writeExpression _ = toRequest @('WriteTable table record)

runWriteTable' :: forall table record client
  .  (WriteTableInterpretation table record, ChClient client)
  => client
  -> Proxy table
  -> [record]
  -> (ChResponse ('WriteTable table record) -> IO ())
  -> IO ()
runWriteTable' client pTable records action = do
  response <- runClient client $ writeExpression pTable records
  action response
  pure $ fromResponse response

runWriteTable :: forall table record client
  .  (WriteTableInterpretation table record, ChClient client)
  => client
  -> Proxy table
  -> [record]
  -> IO ()
runWriteTable client pTable records = runWriteTable' client pTable records
  . const $ pure ()


-- ** Raw Request

instance InterpretExpression 'Raw where
  type InterpretRequest 'Raw = LazyByteString -> ChRequest 'Raw
  type InterpretResponse 'Raw = LazyByteString
  toRequest = MkChRequest
  fromResponse (MkChResponse body) = body

type RawExpressionInterpretation =
  ( InterpretExpression 'Raw
  , InterpretRequest 'Raw ~ (LazyByteString -> ChRequest 'Raw)
  , InterpretResponse 'Raw ~ LazyByteString
  )

rawExpression :: RawExpressionInterpretation
  => LazyByteString
  -> ChRequest 'Raw
rawExpression = MkChRequest

runRawExpression' :: forall client
  .  (RawExpressionInterpretation, ChClient client)
  => client
  -> LazyByteString
  -> IO LazyByteString
runRawExpression' client expression = do
  resp <- runClient client $ rawExpression expression
  pure $ fromResponse resp

runRawExpression :: forall client
  .  (RawExpressionInterpretation, ChClient client)
  => client
  -> LazyByteString
  -> IO ()
runRawExpression client expression =
  void . runClient client $ rawExpression expression


-- * Clients abstraction

class ChClient client where
  type ClientSettings client = settings | settings -> client
  initClient :: ChCredential -> ClientSettings client -> IO client
  runClient :: client -> ChRequest expression -> IO (ChResponse expression)

data ChCredential = MkChCredential
  { chLogin    :: !Text
  , chPass     :: !Text
  , chUrl      :: !Text
  , chDatabase :: !Text
  }
  deriving (Generic, NFData, Show, Eq)


-- ** HTTP ClickHouse client realization

data HttpChClient = MkHttpChClient H.Manager H.Request

type HttpChClientSettings = H.ManagerSettings

instance ChClient HttpChClient where
  type ClientSettings HttpChClient = HttpChClientSettings

  initClient (MkChCredential login pass url databaseName) manager = do
    man <- H.newManager manager
    req <- H.parseRequest (T.unpack url)

    pure $! MkHttpChClient
      man
      req
        { H.method         = "POST"
        , H.requestHeaders =
          [ ("X-ClickHouse-User", encodeUtf8 login)
          , ("X-ClickHouse-Key", encodeUtf8 pass)
          , ("X-ClickHouse-Database", encodeUtf8 databaseName)
          ]
          <> H.requestHeaders req
        }

  runClient (MkHttpChClient man req) (MkChRequest expression) = do
    let req' = req{
      H.requestBody = RequestBodyStreamChunked $ \np -> do
        ibss <- newIORef $ BL.toChunks expression
        np $ do
          bss <- readIORef ibss
          case bss of
            [] -> return BS.empty
            bs:bss' -> do
              writeIORef ibss bss'
              return bs
      }
    response <- H.httpLbs req' man
    throwOnNon200 response
    pure . MkChResponse $ H.responseBody response

defaultHttpChClientSettings :: HttpChClientSettings
defaultHttpChClientSettings = H.defaultManagerSettings{managerResponseTimeout = H.responseTimeoutNone}


-- ** HTTP codes handling

newtype ChException = MkChException
  { exceptionMessage :: Text
  }
  deriving (Show)
  deriving anyclass (Exception)

throwOnNon200 :: Response LazyByteString -> IO ()
throwOnNon200 resp =
  when
    (H.statusCode (responseStatus resp) /= 200)
    (throw . MkChException . (T.decodeUtf8 . BS.toStrict . H.responseBody) $ resp)
