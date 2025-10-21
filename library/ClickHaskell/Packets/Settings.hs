module ClickHaskell.Packets.Settings where

-- Internal
import ClickHaskell.Primitive

import GHC.Generics

-- * Server settings

data DbSettings = MkDbSettings [DbSetting]
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
    str <- deserialize @ChString rev
    case str of
      "" -> pure $ MkDbSettings []
      _ -> pure $ MkDbSettings []

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
