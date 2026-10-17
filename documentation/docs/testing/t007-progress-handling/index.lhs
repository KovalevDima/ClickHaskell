```haskell
{-# LANGUAGE
    AllowAmbiguousTypes
  , DataKinds
  , DeriveGeneric
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , TypeFamilies
  , TypeApplications
  , UndecidableInstances
  , ScopedTypeVariables
#-}

module Main (main) where

import ClickHaskell
import Data.String (fromString)

main :: IO ()
main = do
  connection <- openConnection defaultConnectionArgs
  command connection (createTableQuery "progress1")
  command connection (createTableQuery "progress2")
  command connection "INSERT INTO progress1 SELECT * FROM progress1"

  putStrLn "Ok"


createTableQuery :: String -> Command
createTableQuery tableName = fromString $ 
  "CREATE TABLE IF NOT EXISTS " <> tableName <> " (\
  \ `int8` Int8 \ 
  \) \
  \ENGINE = MergeTree \
  \PARTITION BY () \
  \ORDER BY ();\
  \"
```
