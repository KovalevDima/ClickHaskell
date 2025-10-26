{-# LANGUAGE TypeAbstractions #-}
module ClickHaskell.Packets.Settings where

-- Internal
import ClickHaskell.Primitive

-- GHC
import Control.Applicative (liftA2)
import Data.Binary.Builder (Builder)
import Data.Binary.Get (Get, lookAhead, skip)
import Data.ByteString as BS (null)
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prelude hiding (liftA2)

-- * Server settings

data DbSettings = MkDbSettings [DbSetting]

addSetting :: DbSetting -> DbSettings -> DbSettings
addSetting setting (MkDbSettings list) = MkDbSettings (setting : list)

data DbSetting =
  MkDbSetting
    { setting    :: ChString 
    , flags      :: Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
    , value      :: SettingType
    }

instance Serializable DbSetting where
  deserialize rev = do
    setting <- deserialize @ChString rev
    flags <- deserialize @(Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS) rev
    case lookupSetting setting of
      Nothing -> fail ("Unsupported option " <> show setting)
      Just MkSettingDescriptor{deserializer} -> do
        value <- deserializer rev
        pure $ MkDbSetting{..} 
  serialize rev MkDbSetting{setting, flags, value} =
    case lookupSetting setting of
      Nothing -> error "Impossible"
      Just MkSettingDescriptor{serializer} ->
        serialize rev setting
        <> serialize rev flags
        <> serializer rev value

instance Serializable DbSettings where
  serialize rev (MkDbSettings setts) = do
    foldMap (serialize @DbSetting rev) setts
    <> serialize @ChString rev ""
  deserialize rev = do
    (MkChString setting) <- lookAhead (deserialize @ChString rev)
    if BS.null setting
      then skip 1 *> pure (MkDbSettings [])
      else liftA2
        addSetting
        (deserialize @DbSetting rev)
        (deserialize @DbSettings rev)

data Flags = IMPORTANT | CUSTOM | TIER
instance Serializable Flags where
  serialize rev flags =
    serialize @UInt8 rev $
      case flags of
        IMPORTANT -> 0x01
        CUSTOM -> 0x02
        TIER -> 0x0c
  deserialize rev = do
    flagCode <- deserialize @UInt8 rev
    case flagCode of
      0x01 -> pure IMPORTANT
      0x02 -> pure CUSTOM
      0x0c -> pure TIER
      _ -> fail "Unknown flag code"


data SettingDescriptor  = 
  MkSettingDescriptor
    { deserializer :: ProtocolRevision -> Get SettingType
    , serializer   :: ProtocolRevision -> SettingType -> Builder
    }

lookupSetting :: ChString -> Maybe SettingDescriptor
lookupSetting name = lookup name settingsMap

type SettingsMap = [(ChString, SettingDescriptor)]

data SettingType where
  SettingUInt64 :: UInt64 -> SettingType

class
  Serializable settType
  =>
  IsSettingType settType
  where
  toSettingType :: settType -> SettingType
  fromSettingType :: SettingType -> settType

instance IsSettingType UInt64 where
  toSettingType uint64 = SettingUInt64 uint64
  fromSettingType (SettingUInt64 uint64) = uint64

mkSettingDesc :: forall name settType . (IsSettingType settType, KnownSymbol name) => (ChString, SettingDescriptor)
mkSettingDesc = 
  let name = toChType (symbolVal @name Proxy)
      desc = MkSettingDescriptor
        { deserializer = \rev -> toSettingType <$> deserialize @settType rev
        , serializer = \rev -> serialize @settType rev . fromSettingType
        }
  in (name, desc)

settingsMap :: SettingsMap
settingsMap =
  [ mkSettingDesc @"max_threads" @UInt64
  , mkSettingDesc @"max_memory_usage" @UInt64
  , mkSettingDesc @"join_use_nulls" @UInt64
  ]
