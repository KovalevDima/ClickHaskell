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

module ChProtocolDocs (doc) where

import ClickHaskell
  ( ExceptionPacket (..),
    HelloResponse (..),
    PasswordComplexityRules (..),
    ProfileInfo (..),
    ProgressPacket (..),
    TableColumns (..),
    ServerPacket (..),
    IsChType (chTypeName)
  )
import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 (table, td, tr)


class ToDocPart docPart where
  default toDocPart :: (Generic docPart, GToDocPart (Rep docPart)) =>  Html
  toDocPart :: Html
  toDocPart = table (gToDocPart @(Rep docPart)) 

deriving instance ToDocPart ExceptionPacket
deriving instance ToDocPart HelloResponse
deriving instance ToDocPart PasswordComplexityRules
deriving instance ToDocPart ProfileInfo
deriving instance ToDocPart ProgressPacket
deriving instance ToDocPart TableColumns

{-
>>> writeFile "./local.html" doc
-}
doc :: String
doc = renderHtml $ toDocPart @ExceptionPacket




class GToDocPart (f :: Type -> Type) where
  gToDocPart :: Html

instance GToDocPart f => GToDocPart (D1 c (C1 c2 f)) 
  where
  gToDocPart = gToDocPart @f

instance (GToDocPart left, GToDocPart right) => GToDocPart (left :*: right)
  where
  gToDocPart = gToDocPart @left <> gToDocPart @right

instance  
  (KnownSymbol name, IsChType chType)
  =>
  GToDocPart ((S1 (MetaSel (Just name) a b f)) (Rec0 chType)) where
  gToDocPart = (tr . mconcat)
    [ (td . string) (symbolVal @name Proxy)
    , (td . string) (chTypeName @chType)
    ]
