{-# LANGUAGE OverloadedStrings #-}
module System.Service.Power
    (
    -- * Addressing
      upowerService
    , upowerPath
    , upowerDeviceInterface
    ) where

import Network.DBus

upowerService :: BusName
upowerService = "org.freedesktop.UPower"

upowerPath :: ObjectPath
upowerPath = "/org/freedesktop/UPower"

upowerDeviceInterface :: Interface
upowerDeviceInterface = "org.freedesktop.UPower.Device"
