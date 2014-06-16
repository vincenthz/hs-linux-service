{-# LANGUAGE OverloadedStrings #-}
module System.Service.Common
    (
    -- * Addressing
      propertyInterface
    -- * Methods
    , propertyGet
    , propertyGetAll
    , propertySet
    ) where

import Data.String
import Network.DBus
import Control.Applicative

propertyInterface :: Interface
propertyInterface = "org.freedesktop.DBus.Properties"

-- | Get a property value at a specific location, on a service
propertyGet :: DBusConnection -- ^ DBus connection
            -> BusName        -- ^ service to contact
            -> ObjectPath     -- ^ Object path of the property
            -> String         -- ^ Property name
            -> IO DBusValue   -- ^ value of the property
propertyGet con destService destPath property =
    onReturn <$> call con destService dcall
    where dcall = DBusCall destPath "Get" (Just propertyInterface)
                    [DBusString $ fromString (unBusName destService), DBusString $ fromString property]
          onReturn ret = case returnBody ret of
                            [ DBusVariant o ] -> o
                            l                 -> error (show l)

-- | Get all properties and values at a specific location, on a service
propertyGetAll :: DBusConnection -- ^ DBus connection
               -> BusName        -- ^ service to contact
               -> ObjectPath     -- ^ Object path of the property
               -> IO [(String,DBusValue)] -- ^ all properties and values at this location
propertyGetAll con destService destPath =
    onReturn <$> call con destService dcall
    where dcall = DBusCall destPath "GetAll" (Just propertyInterface)
                    [DBusString $ fromString (unBusName destService)]
          onReturn ret = case returnBody ret of
                            [ DBusArray _ a ] -> map unwrapDictStringVariant a
                            l                 -> error (show l)
          unwrapDictStringVariant (DBusDict (DBusString s) (DBusVariant v)) = (packedStringToString s,v)
          unwrapDictStringVariant _ = error "unexpected type, expecting dictionary"

-- | Set a property at a specific location to a value
propertySet :: DBusConnection -- ^ DBus connection
            -> BusName        -- ^ service to contact
            -> ObjectPath     -- ^ Object path of the property
            -> String         -- ^ property name
            -> DBusValue      -- ^ value to set
            -> IO ()
propertySet con destService destPath property val =
    onReturn <$> call con destService dcall
    where dcall = DBusCall destPath "Set" (Just propertyInterface)
                    [DBusString $ fromString (unBusName destService), DBusString $ fromString property, DBusVariant val]
          onReturn _ = ()
