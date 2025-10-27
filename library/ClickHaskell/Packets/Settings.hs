module ClickHaskell.Packets.Settings where

-- Internal
import ClickHaskell.Primitive

-- GHC
import Control.Applicative (liftA2)
import Data.Binary.Builder (Builder)
import Data.Binary.Get (Get, lookAhead, skip)
import Data.ByteString as BS (null)
import Data.Kind (Type)
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude hiding (liftA2)

-- * Server settings

data DbSettings = MkDbSettings [DbSetting]

addSetting :: DbSetting -> DbSettings -> DbSettings
addSetting setting (MkDbSettings list) = MkDbSettings (setting : list)

data DbSetting = MkDbSetting
  { setting    :: ChString 
  , flags      :: Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
  , value      :: SettingType
  }

instance Serializable DbSetting where
  deserialize rev = do
    setting <- deserialize @ChString rev
    flags <- deserialize @(Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS) rev
    case lookupSetting setting of
      Nothing -> fail ("Unsupported setting " <> show setting)
      Just MkSettingSerializer{deserializer} -> do
        value <- deserializer rev
        pure $ MkDbSetting{..} 
  serialize rev MkDbSetting{setting, flags, value} =
    case lookupSetting setting of
      Nothing -> error "Impossible. Unknown setting was added to query packet"
      Just MkSettingSerializer{serializer} ->
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


data SettingSerializer  = 
  MkSettingSerializer
    { deserializer :: ProtocolRevision -> Get SettingType
    , serializer   :: ProtocolRevision -> SettingType -> Builder
    }

lookupSetting :: ChString -> Maybe SettingSerializer
lookupSetting name = lookup name (settingsMap @SupportedSettings)

class SettingsMapBuilder (list :: [Type]) where
  settingsMap :: [(ChString, SettingSerializer)]

instance SettingsMapBuilder '[] where settingsMap = []
instance
  (SettingsMapBuilder xs, IsSettingType settType, KnownSymbol name)
  =>
  SettingsMapBuilder (Setting name settType ': xs)
  where
  settingsMap =
    let name = toChType (symbolVal @name Proxy)
        desc = MkSettingSerializer
          { deserializer = \rev -> toSettingType <$> deserialize @settType rev
          , serializer = \rev -> serialize @settType rev . fromSettingType
          }
    in (name, desc) : settingsMap @xs

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

data Setting (a :: Symbol) (settType :: Type)

type SupportedSettings = '[
    Setting "max_threads_for_indexes" UInt64,
    Setting "max_memory_usage" UInt64
  ]
