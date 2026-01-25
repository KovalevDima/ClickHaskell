{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}

module Main (main) where

import CMark
import ClickHaskell
import ClickHaskell.Protocol
import Data.Kind
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text as T (Text, intercalate, justifyLeft, length, pack, singleton)
import Data.Text.IO as T (writeFile)
import GHC.Generics
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)
import System.Directory
import Prelude hiding (div)

main :: IO ()
main = do
  let dir = "./documentation/docs/protocol/"
  createDirectoryIfMissing True dir
  T.writeFile (dir <> "common.mdx") commonDoc
  T.writeFile (dir <> "server.mdx") serverDoc
  T.writeFile (dir <> "client.mdx") clientDoc


commonDoc :: Text
commonDoc = (nodeToCommonmark [] Nothing) $
  Node Nothing DOCUMENT $ mconcat $
    [ toDocPartMd @DataPacket
    , toDocPartMd @BlockInfo
    ]

deriving instance ToDocPart DataPacket
deriving instance ToDocPart BlockInfo

serverDoc :: Text
serverDoc = (nodeToCommonmark [] Nothing) $
  Node Nothing DOCUMENT $ mconcat $
    [ toDocPartMd @ExceptionPacket
    , toDocPartMd @HelloResponse
    , toDocPartMd @PasswordComplexityRules
    , toDocPartMd @ProfileInfo
    , toDocPartMd @ProgressPacket
    , toDocPartMd @TableColumns
    ]

deriving instance ToDocPart ExceptionPacket
deriving instance ToDocPart HelloResponse
deriving instance ToDocPart PasswordComplexityRules
deriving instance ToDocPart ProfileInfo
deriving instance ToDocPart ProgressPacket
deriving instance ToDocPart TableColumns

clientDoc :: Text
clientDoc = (nodeToCommonmark [] Nothing) $
  Node Nothing DOCUMENT $ mconcat $
    [ toDocPartMd @HelloPacket
    , toDocPartMd @QueryPacket
    , toDocPartMd @ClientInfo
    ]

deriving instance ToDocPart HelloPacket
deriving instance ToDocPart QueryPacket
deriving instance ToDocPart ClientInfo

data Line (lcs :: [Symbol]) where
  NoLines :: Line '[]
  AddLine :: LineContent l -> Line lcs -> Line (lc ': lcs)

data LineContent (t :: Symbol) = MkLineContent Text


class MkHeader (lineSpec :: [Symbol]) where
  mkHeader :: Line lineSpec
  mkEmpty :: Line lineSpec

instance MkHeader '[] where
  mkHeader = NoLines
  mkEmpty = NoLines

instance
  ( KnownSymbol field
  , MkHeader fields
  ) => MkHeader (field ': fields) where
  mkHeader = AddLine (MkLineContent . T.pack . symbolVal @field $ Proxy) (mkHeader @fields)
  mkEmpty = AddLine (MkLineContent "") (mkEmpty @fields)

renderGhMdTable :: forall lcs . MkHeader lcs => [Line lcs] -> Text
renderGhMdTable ls =
  let
    header = mkHeader @lcs
    sizes = getMaxLinesLen (header : ls)
  in
    (render . formatLine sizes ' ') header <>
    (render . formatLine sizes '-') (mkEmpty @lcs) <>
    (mconcat . map (renderLine sizes ' ')) ls
  where
  renderLine sizes sym = render . formatLine sizes sym

  render :: [Text] -> Text
  render x = "|" <> T.intercalate "|" x <> "|\n"

  formatLine :: forall l . [Int] -> Char -> Line l -> [Text]
  formatLine sizes sym line =
    let
      (size, restSizes) = fromMaybe (0, []) (uncons sizes)
      symText = T.singleton sym
    in case line of
      NoLines -> []
      AddLine (MkLineContent txt) restLines ->
        symText <> justifyLeft size sym txt <> symText
          : formatLine restSizes sym restLines

  getMaxLinesLen :: [Line lcs] -> [Int]
  getMaxLinesLen =
    \case
      [] -> []
      [l] -> lineMaxLengths l
      l:lns -> zipWith max (lineMaxLengths l) (getMaxLinesLen lns)
    where
    lineMaxLengths :: Line l -> [Int]
    lineMaxLengths (AddLine (MkLineContent txt) lcs) = T.length txt : lineMaxLengths lcs
    lineMaxLengths NoLines = []


toDocPartMd :: forall docPart . ToDocPart docPart => [Node]
toDocPartMd = [
    Node Nothing (HEADING 1) [Node Nothing (TEXT $ tableName @docPart) []],
    Node Nothing (CUSTOM_BLOCK "" (docTable @docPart)) []
  ]


class ToDocPart docPart where
  default tableName :: (Generic docPart, GToDocPart (Rep docPart)) => Text
  tableName :: Text
  tableName = gTableName @(Rep docPart)

  default docTable :: (Generic docPart, GToDocPart (Rep docPart)) => Text
  docTable :: Text
  docTable = renderGhMdTable $ gDocTable @(Rep docPart)

class GToDocPart (f :: Type -> Type) where
  gTableName :: Text
  gTableName = ""

  gDocTable :: [Line '["Field", "type", "since"]]

instance
  (GToDocPart f, KnownSymbol name)
  =>
  GToDocPart (D1 (MetaData name m p nt) (C1 c2 f)) 
  where
  gTableName = T.pack $ symbolVal @name Proxy
  gDocTable = gDocTable @f

instance (GToDocPart left, GToDocPart right) => GToDocPart (left :*: right)
  where
--  gToDocPart = gToDocPart @left <> gToDocPart @right
  gDocTable = gDocTable @left <> gDocTable @right

instance  
  (KnownSymbol name, HasDesc chType)
  =>
  GToDocPart ((S1 (MetaSel (Just name) a b f)) (Rec0 chType)) where
  gDocTable =
    [ AddLine (MkLineContent $ T.pack . symbolVal @name $ Proxy)
    . AddLine (MkLineContent $ fieldName @chType)
    . AddLine (MkLineContent $ fieldSince @chType)
    $ NoLines
    ]




class HasDesc hasName where
  fieldName :: Text

  fieldSince :: Text
  fieldSince = "-"


instance (KnownNat rev, HasDesc after) => HasDesc (Revisioned rev () after)
  where
  fieldName = fieldName @after
  fieldSince = T.pack . show . natVal @rev $ Proxy

instance HasDesc UVarInt where fieldName = "UVarInt"
instance HasDesc ProtocolRevision where fieldName = "UVarInt"
instance HasDesc [PasswordComplexityRules] where fieldName = "[PasswordComplexityRules]"
instance HasDesc ClientInfo where fieldName = "ClientInfo"
instance HasDesc QueryKind where fieldName = "QueryKind"
instance HasDesc DbSettings where fieldName = "DbSettings"
instance HasDesc QueryStage where fieldName = "QueryStage"
instance HasDesc QueryParameters where fieldName = "QueryParameters"
instance HasDesc BlockInfo where fieldName = "BlockInfo"
instance HasDesc Jwt where fieldName = "Jwt"

instance {-# OVERLAPPABLE #-} IsChType chType => HasDesc chType where
  fieldName = T.pack $ chTypeName @chType
