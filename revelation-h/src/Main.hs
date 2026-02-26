{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import qualified GUI.About
import           GUI.Actions
import qualified GUI.InfoPane
import           GUI.Menu
import qualified GUI.Tree
import           GUI.Window
import qualified RevelationXML
import qualified Revelation2

-- base
import           Control.Exception (try)
import           Control.Monad (when)

-- bytestring
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- haskell-gi-base
import           Data.GI.Base
import           Data.GI.Base.Utils (whenJust)

-- gi-adwaita
import qualified GI.Adw as Adw

-- gi-gtk
import qualified GI.Gtk as Gtk

-- gi-gio
import qualified GI.Gio as Gio

-- mtl
import           Control.Monad.Except (runExceptT)

-- text
import qualified Data.Text.Encoding as TE

main :: IO ()
main = do
  -- input <- BL.readFile "example.xml"
  -- mbEntries <- runExceptT $ RevelationXML.parseEntries input
  -- entries <- case mbEntries of
  --   Left msg -> do
  --     putStrLn $ "Error: " <> msg
  --     return []
  --   Right entries -> return entries

  runApplicationWindow (ApplicationId "org.gtk.revelation-h") $ do
    infoPane <- GUI.InfoPane.create
    -- treePane <- GUI.Tree.create entries (infoPane.render)
    treePane <- GUI.Tree.create [] (infoPane.render)
    pure ApplicationProperties
      { menu = appMenu
      , actions =
        [ APP_QUIT >== \app -> app.quit
        , FILE_OPEN >== \app -> openFileDialog app treePane
        , APP_ABOUT >== \app -> GUI.About.showAboutDialog app
        ]
      , sidebar = SidebarPage
        { title = "Menu"
        , content = justWidget treePane.view
        }
      , content = ContentPage
        { title = "Revelation-H"
        , subtitle = Just "the password manager"
        , content = justWidget infoPane.view
        }
      }
  where
  justWidget w = Just $ unsafeCastTo Gtk.Widget w

appMenu :: [Section]
appMenu =
  [ Section Nothing
    [ "New" ==> FILE_NEW
    , "Open" ==> FILE_OPEN
    , "Save" ==> FILE_SAVE
    , "Save as..." ==> FILE_SAVE_AS
    , "Change password" ==> FILE_CHANGE_PASSWORD
    ]
  , Section Nothing
    [ "Lock" ==> APP_LOCK
    , "Exit" ==> APP_QUIT
    ]
  , Section Nothing
    [ "About" ==> APP_ABOUT
    ]
  ]

openFileDialog :: Adw.Application -> GUI.Tree.TreePane -> IO ()
openFileDialog app tree = do
  mbWindow <- Gtk.applicationGetActiveWindow app
  whenJust mbWindow $ \window -> do
    dialog <- new Gtk.FileDialog [ #title := "Open File" ]

    Gtk.fileDialogOpen dialog
      (Just window)
      (Nothing :: Maybe Gio.Cancellable)
      (Just ( \_ aresult -> do
              result :: Either GError Gio.File <- try (Gtk.fileDialogOpenFinish dialog aresult)
              case result of
                Right choice -> do
                  path <- #getPath choice
                  whenJust path $ \file -> do
                    withPassword app $ \password -> do
                      openFile file password tree
                Left _ -> pure ()
              return ()
            )
      )

openFile :: FilePath -> B.ByteString -> GUI.Tree.TreePane -> IO ()
openFile inputFileName password tree = do
  input <- BL.readFile inputFileName

  eitherEntries <- runExceptT $ do
    xml <- Revelation2.decrypt input password
    RevelationXML.parseEntries xml
  case eitherEntries of
    Left err -> putStrLn $ "Error: " <> err
    Right entries -> tree.update entries

withPassword :: Adw.Application -> (B.ByteString -> IO ()) -> IO ()
withPassword app callback = do
  dialog <- new Adw.AlertDialog
    [ #heading := "Password"
    , #body := "Enter file password"
    , #defaultResponse := "submit"
    , #closeResponse := "cancel"
    ]
  Adw.alertDialogAddResponse dialog "cancel" "Cancel"
  Adw.alertDialogAddResponse dialog "submit" "Ok"
  Adw.alertDialogSetResponseAppearance dialog "submit" Adw.ResponseAppearanceSuggested

  password <- new Gtk.Entry
    [ #marginStart := 20
    , #marginEnd := 20
    , #marginTop := 10
    , #marginBottom := 10
    , #visibility := False
    , #inputPurpose := Gtk.InputPurposePassword
    , #placeholderText := "Password"
    , #activatesDefault := True -- emit default action on Enter key
    ]
  Adw.alertDialogSetExtraChild dialog $ Just password

  on dialog #response $ \responseId ->
    when (responseId == "submit") $
      callback =<< TE.encodeUtf8 <$> (Gtk.entryBufferGetText =<< Gtk.entryGetBuffer password)

  Adw.dialogPresent dialog =<< Gtk.applicationGetActiveWindow app

  Gtk.widgetGrabFocus password >> return ()
