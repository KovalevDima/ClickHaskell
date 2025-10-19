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

module Main (main) where

import ClickHaskell
import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal, natVal, KnownNat)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 (table, td, tr, h3, thead, tbody, th, div)
import Data.ByteString.Lazy.Char8 as BS8
import System.Directory
import Prelude hiding (div)

main :: IO ()
main = do
  let dir = "./documentation/app/routes/protocol/"
  createDirectoryIfMissing True dir
  BS8.writeFile (dir <> "common.html") commonDoc
  BS8.writeFile (dir <> "server.html") serverDoc
  BS8.writeFile (dir <> "client.html") clientDoc


commonDoc :: ByteString
commonDoc = (BS8.pack . renderHtml . mconcat)
  [ toDocPart @DataPacket
  , toDocPart @BlockInfo
  ]

deriving instance ToDocPart DataPacket
deriving instance ToDocPart BlockInfo

serverDoc :: ByteString
serverDoc = (BS8.pack . renderHtml . mconcat)
  [ toDocPart @ExceptionPacket
  , toDocPart @HelloResponse
  , toDocPart @PasswordComplexityRules
  , toDocPart @ProfileInfo
  , toDocPart @ProgressPacket
  , toDocPart @TableColumns
  ]

deriving instance ToDocPart ExceptionPacket
deriving instance ToDocPart HelloResponse
deriving instance ToDocPart PasswordComplexityRules
deriving instance ToDocPart ProfileInfo
deriving instance ToDocPart ProgressPacket
deriving instance ToDocPart TableColumns

clientDoc :: ByteString
clientDoc = (BS8.pack . renderHtml . mconcat)
  [ toDocPart @HelloPacket
  , toDocPart @QueryPacket
  , toDocPart @ClientInfo
  ]

deriving instance ToDocPart HelloPacket
deriving instance ToDocPart QueryPacket
deriving instance ToDocPart ClientInfo

class ToDocPart docPart where
  default toDocPart :: (Generic docPart, GToDocPart (Rep docPart)) => Html
  toDocPart :: Html
  toDocPart = gToDocPart @(Rep docPart)


class GToDocPart (f :: Type -> Type) where
  gToDocPart :: Html

instance
  (GToDocPart f, KnownSymbol name)
  =>
  GToDocPart (D1 (MetaData name m p nt) (C1 c2 f)) 
  where
  gToDocPart = do
    div $ do
      h3 (string $ symbolVal @name $ Proxy)
      table $ do
        thead $ do
          th (string "Field")
          th (string "type")
          th (string "since")
        tbody (gToDocPart @f)

instance (GToDocPart left, GToDocPart right) => GToDocPart (left :*: right)
  where
  gToDocPart = gToDocPart @left <> gToDocPart @right

instance  
  (KnownSymbol name, GToDocPart rec)
  =>
  GToDocPart ((S1 (MetaSel (Just name) a b f)) rec) where
  gToDocPart = tr $ do
    (td . string) (symbolVal @name Proxy)
    gToDocPart @rec 

instance {-# OVERLAPPABLE #-}
  HasName chType
  =>
  GToDocPart (Rec0 chType) where
  gToDocPart = do
    (td . string) (fieldName @chType)
    (td . string) ("-")

instance  
  (HasName chType, KnownNat rev)
  =>
  GToDocPart (Rec0 (chType `SinceRevision` rev)) where
  gToDocPart = do
    (td . string) (fieldName @chType)
    (td . string) (show $ natVal @rev Proxy)


class HasName hasName where fieldName :: String
instance HasName UVarInt where fieldName = "UVarInt"
instance HasName ProtocolRevision where fieldName = "UVarInt"
instance HasName [PasswordComplexityRules] where fieldName = "[PasswordComplexityRules]"
instance HasName ClientInfo where fieldName = "ClientInfo"
instance HasName QueryKind where fieldName = "QueryKind"
instance HasName DbSettings where fieldName = "DbSettings"
instance HasName QueryStage where fieldName = "QueryStage"
instance HasName QueryParameters where fieldName = "QueryParameters"
instance HasName BlockInfo where fieldName = "BlockInfo"

instance {-# OVERLAPPABLE #-} IsChType chType => HasName chType where 
  fieldName = chTypeName @chType
