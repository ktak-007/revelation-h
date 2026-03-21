{-# LANGUAGE TemplateHaskell #-}

module GUI.Icons ( create ) where

import           RevelationXML

-- rio
import           RIO

-- file-embed
import           Data.FileEmbed (embedFile, makeRelativeToProject)

-- gi-glib
import qualified GI.GLib as GLib

-- gi-gdk
import qualified GI.Gdk as Gdk


getIcon:: Entry -> Bool -> Maybe ByteString
getIcon Folder {} False = Just $(embedFile =<< makeRelativeToProject "icons/folder-symbolic.svg")
getIcon Folder {} True = Just $(embedFile =<< makeRelativeToProject "icons/folder-open-symbolic.svg")
getIcon Generic {} _ = Just $(embedFile =<< makeRelativeToProject "icons/text-x-generic-symbolic.svg")
getIcon Website {} _ = Just $(embedFile =<< makeRelativeToProject "icons/web-browser-symbolic.svg")
getIcon Vnc {} _ = Just $(embedFile =<< makeRelativeToProject "icons/computer-symbolic.svg")
getIcon Shell {} _ = Just $(embedFile =<< makeRelativeToProject "icons/utilities-terminal-symbolic.svg")
getIcon Email {} _ = Just $(embedFile =<< makeRelativeToProject "icons/mail-symbolic.svg")
getIcon Creditcard {} _ = Just $(embedFile =<< makeRelativeToProject "icons/credit-card-symbolic.svg")
getIcon Phone {} _ = Just $(embedFile =<< makeRelativeToProject "icons/smartphone-symbolic.svg")
getIcon Door {} _ = Just $(embedFile =<< makeRelativeToProject "icons/changes-allow-symbolic.svg")
getIcon Cryptokey {} _ = Just $(embedFile =<< makeRelativeToProject "icons/dialog-password-symbolic.svg")
getIcon Ftp {} _ = Just $(embedFile =<< makeRelativeToProject "icons/system-file-manager-symbolic.svg")
getIcon Database {} _ = Just $(embedFile =<< makeRelativeToProject "icons/database-symbol-svgrepo-com.svg")

create :: Entry -> Bool -> IO (Maybe Gdk.Texture)
create entry open = case getIcon entry open of
  Nothing -> return Nothing
  justIcon -> Just <$> (Gdk.textureNewFromBytes =<< GLib.bytesNew justIcon)
