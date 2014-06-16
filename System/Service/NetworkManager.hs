{-# LANGUAGE OverloadedStrings #-}
module System.Service.NetworkManager
    (
    -- * Addressing
      networkmanagerService
    , networkmanagerPath
    , networkmanagerInterface
    , networkmanagerDevicesInterface
    -- * Methods
    , getDevices
    , getState
    , deactivateConnection
    , activateConnection
    ) where

import Control.Applicative
import Data.Word
import Network.DBus
import System.Service.Common

networkmanagerService :: BusName
networkmanagerService = "org.freedesktop.NetworkManager"

networkmanagerPath :: ObjectPath
networkmanagerPath = "/org/freedesktop/NetworkManager"

networkmanagerInterface :: Interface
networkmanagerInterface = "org.freedesktop.NetworkManager"

networkmanagerDevicesInterface :: Interface
networkmanagerDevicesInterface = "org.freedesktop.NetworkManager.Devices"

-- | Call to the network manager
{-
networkManagerCall :: DBusConnection
                   -> Member
                   -> Maybe Interface
                   -> Body
                   -> IO DBusReturn
-}
networkManagerCall con method intf params =
    call con networkmanagerService $ DBusCall networkmanagerPath method intf params

onReturnVoid :: DBusReturn -> ()
onReturnVoid ret =
    case returnBody ret of
        [] -> ()
        _  -> error "not expecting any return value, got some"

-- | Get all devices Object Path.
getDevices :: DBusConnection -> IO [ObjectPath]
getDevices con =
    onReturn <$> networkManagerCall con "GetDevices" Nothing {-(Just networkmanagerInterface)-} []
    where onReturn ret = case returnBody ret of
                            [DBusArray SigObjectPath l] -> map unString l
                            _                       -> error "unexpected type, excepting array of string"
          unString (DBusObjectPath s) = s
          unString _                  = error "unexpected type, expecting string"


getState :: DBusConnection -> IO Word32
getState con = toState <$> propertyGet con networkmanagerService networkmanagerPath "State"
    where toState (DBusUInt32 u) = u
          toState _              = error "Bad State type"

-- | Deactivate a connection specified by its object path.
deactivateConnection :: DBusConnection -> ObjectPath -> IO ()
deactivateConnection con obj =
    onReturnVoid <$> networkManagerCall con "DeactivateConnection" (Just networkmanagerInterface)
        [ DBusObjectPath obj ]

-- | Activate a connection
activateConnection :: DBusConnection -- ^ DBus Context
                   -> ObjectPath     -- ^ Connection
                   -> ObjectPath     -- ^ Device
                   -> ObjectPath     -- ^ Specific object
                   -> IO ObjectPath  -- ^ Active connection object
activateConnection con connection device specificObject =
    onReturn <$> networkManagerCall con "ActivateConnection" (Just networkmanagerInterface)
        [ DBusObjectPath connection
        , DBusObjectPath device
        , DBusObjectPath specificObject
        ]
    where onReturn ret = case returnBody ret of
                              [DBusObjectPath obj] -> obj
                              _                    -> error "unexpected type, expecting objectpath"

--onStateChange :: DBusConnection -> (() -> IO ()) -> IO ()
--onStateChange con callback
