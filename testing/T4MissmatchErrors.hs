{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , TypeApplications
  , ScopedTypeVariables
#-}

module T4MissmatchErrors where

-- Internal
import ClickHaskell

-- GHC included
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Exception (catch, throw)
import Control.Monad (void)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)


t4 :: HasCallStack => Connection -> IO ()
t4 connection = do
  catch (
    void $
      select
        @ExpectedColumns
        @ExpectedName
        connection
        (toChType "SELECT * FROM generateRandom('unexpectedName Int64', 1, 10, 2) LIMIT 1")
        pure
    )
    (\(e :: ClientError) -> case e of
      UserError (UnmatchedColumn _) -> pure ()
      e -> error ("MissmatchErrors: " <> show e)
    )

  catch (
    void $
      select
        @ExpectedColumns
        @ExpectedName
        connection
        (toChType "SELECT * FROM generateRandom('expectedName Int64', 1, 10, 2) LIMIT 1")
        pure
    )
    (\(e :: ClientError) -> case e of
      UserError (UnmatchedType _) -> pure ()
      e -> error ("MissmatchErrors: " <> show e)
    )

  print "MissmatchErrors: Ok"


data ExpectedName = MkExpectedName
  { expectedName :: ChInt64
  }
  deriving (Generic)
  deriving anyclass (ReadableFrom (Columns ExpectedColumns))


type ExpectedColumns =
 '[ Column "expectedName" ChInt64
  ]
