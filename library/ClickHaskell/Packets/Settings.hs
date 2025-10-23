module ClickHaskell.Packets.Settings where

-- Internal
import ClickHaskell.Primitive

-- GHC
import Data.Binary (Get)
import Data.Binary.Builder (Builder)
import Data.Typeable (Proxy (..))
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

-- * Server settings

data DbSettings = MkDbSettings [DbSetting]

addSetting :: DbSetting -> DbSettings -> DbSettings
addSetting setting (MkDbSettings list) = MkDbSettings (setting : list)

data DbSetting = MkDbSetting
  { setting :: ChString
  , flags   :: Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
  , value   :: ChString
  }
  deriving (Generic, Serializable)

instance Serializable DbSettings where
  serialize rev (MkDbSettings setts) = do
    foldMap (serialize @DbSetting rev) setts
    <> serialize @ChString rev ""
  deserialize rev = do
    setting <- deserialize @ChString rev
    case setting of
      "" -> pure $ MkDbSettings []
      _ -> do
        _ <- fail "Settings deserialization unsupported"
        flags <- deserialize rev
        value <- deserialize rev
        addSetting MkDbSetting{..}
          <$> deserialize @DbSettings rev

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

data SettingValue = forall a . Serializable a => SettingValue a

data SettingDescriptor = forall a. Serializable a =>
  SettingDescriptor
    { deserializer :: ProtocolRevision -> Get a
    , serializer :: ProtocolRevision -> a -> Builder
    }

type SettingMap = [(ChString, SettingDescriptor)]

settingMap :: SettingMap
settingMap =
  [ mkSettingDesc @"max_threads" @UInt64
  , mkSettingDesc @"max_memory_usage" @UInt64
  , mkSettingDesc @"join_use_nulls" @UInt64
  ]

mkSettingDesc :: forall name settType . (Serializable settType, KnownSymbol name) => (ChString, SettingDescriptor)
mkSettingDesc = 
  let name = toChType (symbolVal @name Proxy)
      desc = SettingDescriptor (deserialize @settType) (serialize @settType)
  in (name, desc)

deserializeSettingsMap :: ProtocolRevision -> Get [(ChString, SettingValue)]
deserializeSettingsMap rev  = go []
  where
    go acc = do
      name <- deserialize @ChString rev
      if (name == "")
      then do
        _flags <- deserialize @(Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS) rev
        case lookup name settingMap of
          Just (SettingDescriptor deserializer _serializer) -> do
            val <- deserializer rev
            go ((name, (SettingValue val)) : acc)
          Nothing -> do
            strVal <- deserialize @ChString rev
            go ((name, (SettingValue strVal)): acc)
      else pure acc
