{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.String
import Network.DBus
import Network.DBus.Actions
import Data.Word
import Data.Int
import Control.Applicative

import System.Service.Common
import System.Service.NetworkManager
import System.Service.Notification

main = do
    con <- establish busGetSession authenticateWithRealUID
    --notify con "My app" Nothing "" "Summary" "Body" (-1)

    conSys <- establish busGetSystem authenticateWithRealUID
    r <- propertyGet conSys networkmanagerService networkmanagerPath "WirelessEnabled"
    putStrLn $ show r
    --propertySet conSys networkmanagerService networkmanagerPath "WirelessEnabled" (DBusBoolean True)
    r <- propertyGetAll conSys networkmanagerService networkmanagerPath
    putStrLn $ show r

    --r <- propertyGetAll conSys networkmanagerService networkmanagerPath
    --putStrLn $ show r

    devs <- getDevices conSys 
    putStrLn $ show devs

    st <- getState conSys 
    putStrLn $ show st
