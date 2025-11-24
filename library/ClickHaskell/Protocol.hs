module ClickHaskell.Protocol
  (
  {- * Protocol parts -}

  {- ** Shared -}
  UVarInt(..), Revisioned(..), ProtocolRevision
  {- *** Data packet -}, DataPacket(..), BlockInfo(..)

  {- ** Client -}, ClientPacket(..)
  {- *** Hello -}, HelloPacket(..), Addendum(..)
  {- *** Query -}
  , QueryPacket(..)
  , DbSettings(..), QueryParameters(..), QueryStage(..)
  , ClientInfo(..), QueryKind(..), Jwt(..)
  
  {- ** Server -}, ServerPacket(..)
  {- *** Hello -}, HelloResponse(..), PasswordComplexityRules(..)
  {- *** Exception -}, ExceptionPacket(..)
  {- *** Progress -}, ProgressPacket(..)
  {- *** ProfileInfo -}, ProfileInfo(..)
  {- *** TableColumns -}, TableColumns(..)

  {- * API -}
  , addSetting
  , mkDataPacket
  , mkQueryPacket, QueryPacketArgs(..)
  , mkHelloPacket
  , mkAddendum
  , serverPacketToNum
  ) where

import ClickHaskell.Primitive
import ClickHaskell.Protocol.Client
import ClickHaskell.Protocol.Data
import ClickHaskell.Protocol.Server
import ClickHaskell.Protocol.Settings
