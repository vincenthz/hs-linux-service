{-# LANGUAGE OverloadedStrings #-}
module System.Service.Notification
    (
    -- * Addressing
      notificationService
    , notificationPath
    -- * Types
    , NotificationID
    -- * Methods
    , notify
    ) where

import Data.String
import Network.DBus
import Data.Word
import Data.Int
import Control.Applicative

notificationService :: BusName
notificationService = "dbus-org.freedesktop.Notifications"

notificationPath :: ObjectPath
notificationPath = "/org/freedesktop/Notifications"

-- | ID for notification. uniquely identify a notification message
newtype NotificationID = NotificationID { unNotificationID :: Word32 } deriving (Eq)

notify :: DBusConnection
       -> String               -- ^ Application name
       -> Maybe NotificationID -- ^ To override an existing notification if still exists.
       -> String               -- ^ Icon
       -> String               -- ^ Summary of the notification
       -> String               -- ^ Body of the notification
       -> Int32                -- ^ Expiration timeout in milliseconds. -1 is the default, 0 never expire.
       -> IO NotificationID    -- ^ Return the Notification ID, to be able to close or rewrite
notify con appName replacesId appIcon summary body expireTimeout =
    onReturn <$> call con notificationService dcall
    where dcall = DBusCall notificationPath "Notify" (Just "org.freedesktop.Notifications")
                [ DBusString $ fromString appName
                , DBusUInt32 (maybe 0 unNotificationID replacesId)
                , DBusString $ fromString appIcon
                , DBusString $ fromString summary
                , DBusString $ fromString body
                -- actions : Actions are sent over as a list of pairs. Each even element in the list (starting at index 0) represents the identifier for the action. Each odd element in the list is the localized string that will be displayed to the user.
                , DBusArray (SigString) []
                -- Optional hints that can be passed to the server from the client program. Although clients and servers should never assume each other supports any specific hints, they can be used to pass along information, such as the process PID or window ID, that the server may be able to make use of. See Hints. Can be empty.
                , DBusArray (SigDict SigString SigVariant) []
                , DBusInt32 expireTimeout
                ]
          onReturn ret = case returnBody ret of
                            [ DBusUInt32 w ] -> NotificationID w
                            _                -> error "unexpected return"
