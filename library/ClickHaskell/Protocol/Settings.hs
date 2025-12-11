{-# LANGUAGE FunctionalDependencies #-}

module ClickHaskell.Protocol.Settings where

-- Internal
import ClickHaskell.Primitive
import ClickHaskell.Protocol.SettingsSupport (KnownSetting (..), SettingType, IsSettingType (..), SettingSerializer (..), settingsMap)

-- GHC
import Data.Binary.Get (lookAhead)
import Data.Bits
import Data.ByteString as BS (null)
import Data.Typeable (Proxy (..))
import GHC.TypeLits (symbolVal)

-- * Server settings

data DbSettings = MkDbSettings [DbSetting]

addSetting
  :: forall name settType
  . KnownSetting name settType
  => settType
  -> DbSettings
  -> DbSettings
addSetting val (MkDbSettings xs) =
  let setting = toChType (symbolVal @name Proxy)
      flags = AfterRevision fIMPORTANT
      value = toSettingType val
  in MkDbSettings (MkDbSetting{..} : xs)

data DbSetting = MkDbSetting
  { setting    :: ChString 
  , flags      :: Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
  , value      :: SettingType
  }

instance Serializable DbSetting where
  deserialize rev = do
    setting <- deserialize @ChString rev
    flags <- deserialize @(Flags `SinceRevision` DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS) rev
    case lookup setting settingsMap of
      Nothing -> fail ("Unsupported setting " <> show setting)
      Just MkSettingSerializer{deserializer} -> do
        value <- deserializer rev
        pure $ MkDbSetting{..} 
  serialize rev MkDbSetting{setting, flags, value} =
    serialize rev setting
    <> serialize rev flags
    <> case lookup setting settingsMap of
      Nothing -> error "Impossible happened. Unknown setting was added to query packet"
      Just MkSettingSerializer{serializer} -> serializer rev value

instance Serializable DbSettings where
  serialize rev (MkDbSettings setts) =
    foldMap (serialize @DbSetting rev) setts
    <> serialize @ChString rev ""
  deserialize rev = do
    (MkChString setting) <- lookAhead (deserialize @ChString rev)
    if BS.null setting
      then deserialize @ChString rev *> pure (MkDbSettings [])
      else do
        sett <- deserialize @DbSetting rev
        (\(MkDbSettings setts) -> MkDbSettings (sett : setts))
          <$> deserialize @DbSettings rev

-- ** Flags

newtype Flags = MkFlags UVarInt
  deriving newtype (Serializable, Num, Eq, Bits)

-- *** Custom

fCUSTOM :: Flags
fCUSTOM = 0x02

isCustom :: Flags -> Bool
isCustom = (/= 0) . (.&. fCUSTOM)

setCustom :: Flags -> Flags
setCustom = (.|. fCUSTOM)

-- *** Important

fIMPORTANT :: Flags
fIMPORTANT = 0x01

isImportant :: Flags -> Bool
isImportant = (/= 0) . (.&. fIMPORTANT)

setImportant :: Flags -> Flags
setImportant = (.|. fIMPORTANT)

-- *** Tier

fTIER :: Flags
fTIER = 0x0c -- 0b1100 == 2 bits
