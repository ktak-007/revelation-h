{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module GUI.Icons ( create ) where

import           RevelationXML

-- rio
import           RIO

-- file-embed
import           Data.FileEmbed (embedFile, makeRelativeToProject)

#if darwin_BUILD_OS
-- gi-rsvg
import qualified GI.Rsvg as Rsvg
#else
-- gi-glib
import qualified GI.GLib as GLib
#endif

-- gi-gdk
import qualified GI.Gdk as Gdk


getIcon :: Entry -> Bool -> Maybe ByteString
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

#if darwin_BUILD_OS

svgToTexture :: ByteString -> IO Gdk.Texture
svgToTexture svgData = do
  mHandle <- Rsvg.handleNewFromData svgData
  case mHandle of
    Nothing     -> fail "svgToTexture: failed to parse SVG"
    Just h -> do
      mPixbuf <- Rsvg.handleGetPixbufAndError h
      case mPixbuf of
        Nothing     -> fail "svgToTexture: failed to render SVG"
        Just pixbuf -> Gdk.textureNewForPixbuf pixbuf

create :: Entry -> Bool -> IO (Maybe Gdk.Texture)
create entry open = case getIcon entry open of
  Nothing      -> return Nothing
  Just svgData -> Just <$> svgToTexture svgData

#else

create :: Entry -> Bool -> IO (Maybe Gdk.Texture)
create entry open = case getIcon entry open of
  Nothing -> return Nothing
  justIcon -> Just <$> (Gdk.textureNewFromBytes =<< GLib.bytesNew justIcon)

#endif
