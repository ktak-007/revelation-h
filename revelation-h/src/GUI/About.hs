{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI.About
  ( showAboutDialog
  ) where

-- text
import           Data.Text (Text)

-- haskell-gi-base
import           Data.GI.Base

-- gi-adwaita
import qualified GI.Adw as Adw

-- gi-gtk
import qualified GI.Gtk as Gtk

showAboutDialog :: Adw.Application -> IO ()
showAboutDialog app = do
  mbWindow <- Gtk.applicationGetActiveWindow app
  case mbWindow of
    Nothing -> pure ()
    Just window -> do
      about <- new Adw.AboutDialog
        [ #applicationName := ("Revelation-H" :: Text)
        , #applicationIcon := ("dialog-information" :: Text)
        , #version := ("0.1.0.0" :: Text)
        , #comments := ("The password manager" :: Text)
        , #website := ("https://github.com/ktak-007/revelation-h" :: Text)
        , #licenseType := Gtk.LicenseGpl30
        , #copyright := ("© 2026 Stanislav Smirnov" :: Text)
        , #developers := (["Stanislav Smirnov"] :: [Text])
        ]
      Adw.dialogPresent (about :: Adw.AboutDialog) (Just window)
