{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -freduction-depth=1200 #-}

module ClickHaskell.Protocol.Settings where

-- Internal
import ClickHaskell.Primitive
import ClickHaskell.Protocol.SettingsSupport (KnownSetting, SettingType, IsSettingType (..))

-- GHC
import Data.Binary.Builder (Builder)
import Data.Binary.Get (Get, lookAhead)
import Data.Bits
import Data.ByteString as BS (null)
import Data.Kind (Type)
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal, TypeError, ErrorMessage (..))
import Prelude hiding (liftA2)

-- * Server settings

data DbSettings = MkDbSettings [DbSetting]




data Setting (a :: Symbol) (settType :: Type)

{-# DEPRECATED addSetting "Unstable function. Use carefully with old ClickHouse versions" #-}
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
    case lookupSetting setting of
      Nothing -> fail ("Unsupported setting " <> show setting)
      Just MkSettingSerializer{deserializer} -> do
        value <- deserializer rev
        pure $ MkDbSetting{..} 
  serialize rev MkDbSetting{setting, flags, value} =
    serialize rev setting
    <> serialize rev flags
    <> case lookupSetting setting of
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




-- * Serialization internals

data SettingSerializer  = 
  MkSettingSerializer
    { deserializer :: ProtocolRevision -> Get SettingType
    , serializer   :: ProtocolRevision -> SettingType -> Builder
    }


type SupportedSettings = '[]

lookupSetting :: ChString -> Maybe SettingSerializer
lookupSetting name = lookup name (settingsMap @SupportedSettings)

class SettingsMapBuilder (list :: [Type]) where
  settingsMap :: [(ChString, SettingSerializer)]

instance
  SettingsMapBuilder '[]
  where
  settingsMap = []

instance
  (SettingsMapBuilder xs, IsSettingType settType, KnownSymbol name)
  =>
  SettingsMapBuilder (Setting name settType ': xs)
  where
  settingsMap =
    let name = toChType (symbolVal @name Proxy)
        deserializer = \rev ->
          if rev >= mkRev @DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
          then fail "Deserialization of Settings serializaed as strings is unsuported"
          else toSettingType <$> deserialize @settType rev
        serializer = \rev ->
          if rev >= mkRev @DBMS_MIN_REVISION_WITH_SETTINGS_SERIALIZED_AS_STRINGS
          then (serialize @ChString rev . settingToText @settType)
          else serialize @settType rev . fromSettingType
    in (name, MkSettingSerializer{..}) : settingsMap @xs

type family LookupSettingType (name :: Symbol) (settings :: [Type]) :: Type where
  LookupSettingType name '[] = TypeError ('Text "Unknown setting name: " ':<>: 'ShowType name)
  LookupSettingType name (Setting name t ': xs) = t
  LookupSettingType name (_ ': xs) = LookupSettingType name xs
