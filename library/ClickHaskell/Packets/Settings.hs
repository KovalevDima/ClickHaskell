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
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prelude hiding (liftA2)

-- * Server settings

data DbSettings = MkDbSettings [DbSetting]

lookupSetting :: ChString -> Maybe SettingDescriptor
lookupSetting name = lookup name settingsMap

addSetting :: DbSetting -> DbSettings -> DbSettings
addSetting setting (MkDbSettings list) = MkDbSettings (setting : list)

data DbSetting = forall a . Serializable a => MkDbSetting
  { setting :: ChString
  , flags   :: Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
  , value   :: a
  }

instance Serializable DbSetting where
  deserialize rev = do
    
    setting <- deserialize @ChString rev
    flags <- deserilize @Flags
    
    value <- case lookupSetting setting of
      Nothing -> fail ("Unsupported option " <> show setting)
      Just SettingDescriptor{deserializer} -> deserializer rev
    pure $ MkDbSetting{..} 
  serialize rev MkDbSetting{setting,flags,value} = case lookupSetting setting of
    Nothing -> error "Impossible"
    Just (SettingDescriptor{serializer}) -> serialize rev setting <> serialize rev flags <> serializer rev value

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


data SettingDescriptor = forall a. Serializable a =>
  SettingDescriptor
    { deserializer :: ProtocolRevision -> Get a
    , serializer :: ProtocolRevision -> a -> Builder
    }

type SettingsMap = [(ChString, SettingDescriptor)]

settingsMap :: SettingsMap
settingsMap =
  [ mkSettingDesc @"max_threads" @UInt64
  , mkSettingDesc @"max_memory_usage" @UInt64
  , mkSettingDesc @"join_use_nulls" @UInt64
  ]

mkSettingDesc :: forall name settType . (Serializable settType, KnownSymbol name) => (ChString, SettingDescriptor)
mkSettingDesc = 
  let name = toChType (symbolVal @name Proxy)
      desc = SettingDescriptor  @settType (deserialize @settType) (serialize @settType)
  in (name, desc)
