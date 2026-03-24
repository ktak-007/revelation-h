{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Definitions
import qualified GUI.About
import           GUI.Actions
import qualified GUI.InfoPane
import           GUI.Menu
import qualified GUI.Tree
import           GUI.Window

import qualified RevelationXML
import qualified Revelation2

-- base
import           Prelude (putStrLn)
import           System.Environment (getArgs, getProgName)

-- rio
import           RIO hiding (on, openFile, set)
import qualified RIO.ByteString.Lazy as RBL

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


printUsage :: RIO App ()
printUsage = do
    pn <- liftIO getProgName
    logInfo $ displayShow $ "Usage: " ++ pn ++ " [--xml decodedFile]"

runApp :: RIO App () -> IO ()
runApp inner = do
  openedFile <- newTVarIO Nothing
  lo <- logOptionsHandle stderr True
  let logOptions = lo & setLogUseTime False
                      & setLogUseLoc  False
  withLogFunc logOptions $ \logFunc -> do
    let appInfo = App
          { appLogFunc = logFunc
          , applicationId = "org.gtk.revelation-h"
          , openedFile = openedFile
          , changed = False
          }
    runRIO appInfo inner

main :: IO ()
main = runApp $ do
  args <- liftIO getArgs
  entries <- case args of
    [] -> return []
    ["--help"] -> printUsage >> exitSuccess
    ["--xml", xmlFile] -> do
      input <- liftIO $ BL.readFile xmlFile
      mbEntries <- liftIO $ runExceptT $ RevelationXML.parseEntries input
      entries <- case mbEntries of
        Left msg -> (logError $ displayShow msg) >> return []
        Right entries -> return entries
      return entries
    _ -> printUsage >> exitFailure

  runApplicationWindow $ do
    infoPane <- GUI.InfoPane.create
    treePane <- GUI.Tree.create entries (infoPane.render)
    pure ApplicationProperties
      { menu = appMenu
      , actions =
        [ APP_QUIT >== \app -> app.quit
        , FILE_OPEN >== \app -> openFileDialog app treePane
        , FILE_SAVE_AS >== \app -> showSaveDialog app (encodeAndSave entries)
        , FILE_SAVE >== \app -> fileSave app (encodeAndSave entries)
        , APP_ABOUT >== \app -> liftIO $ GUI.About.showAboutDialog app
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

openFileDialog :: Adw.Application -> GUI.Tree.TreePane -> RIO App ()
openFileDialog app tree = do
  mbWindow <- Gtk.applicationGetActiveWindow app
  whenJust mbWindow $ \window -> do
    appInfo <- ask
    dialog <- new Gtk.FileDialog [ #title := "Open File" ]

    Gtk.fileDialogOpen dialog
      (Just window)
      (Nothing :: Maybe Gio.Cancellable)
      $ Just $ \_ aresult -> do
        result :: Either GError Gio.File <- try (Gtk.fileDialogOpenFinish dialog aresult)
        case result of
          Right choice -> do
            path <- #getPath choice
            whenJust path $ \file ->
              runRIO appInfo $ withPassword app $ \password ->
                openFile file password tree
          Left _ -> pure ()
        return ()

openFile :: FilePath -> B.ByteString -> GUI.Tree.TreePane -> RIO App ()
openFile inputFileName password tree = do
  App {..} <- ask
  input <- liftIO $ BL.readFile inputFileName

  eitherEntries <- liftIO $ runExceptT $ do
    xml <- Revelation2.decrypt input password
    RevelationXML.parseEntries xml
  case eitherEntries of
    Left err -> liftIO $ putStrLn $ "Error: " <> err
    Right entries -> do
      liftIO $ tree.update entries
      atomically $ writeTVar openedFile $ Just $ OpenedFile inputFileName password

withPassword :: Adw.Application -> (B.ByteString -> RIO App ()) -> RIO App ()
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

  appInfo <- ask
  on dialog #response $ \responseId ->
    when (responseId == "submit") $ runRIO appInfo $
      callback =<< TE.encodeUtf8 <$> (Gtk.entryBufferGetText =<< Gtk.entryGetBuffer password)

  Adw.dialogPresent dialog =<< Gtk.applicationGetActiveWindow app

  Gtk.widgetGrabFocus password >> return ()

encodeAndSave :: [RevelationXML.Entry] -> FilePath -> ByteString -> RIO App ()
encodeAndSave entries fileName password = do
  let xml = RevelationXML.render entries
  mbEncodedXML <- liftIO $ runExceptT $ Revelation2.encrypt xml password
  case mbEncodedXML of
    Right encodedXML -> do
      RBL.writeFile fileName $ encodedXML
      openedFile' <- view $ to openedFile
      atomically $ writeTVar openedFile' $ Just $ OpenedFile fileName password
    Left err -> do
      logError $ "Can't encode file: " <> displayShow err
      exitFailure

showSaveDialog :: Adw.Application -> (FilePath -> ByteString -> RIO App ()) -> RIO App ()
showSaveDialog app onSave = do
  mbWindow <- Gtk.applicationGetActiveWindow app
  whenJust mbWindow $ \window -> do
    appInfo <- ask
    dialog <- new Gtk.FileDialog [ #title := "Save file" ]
    Gtk.fileDialogSave dialog
      (Just window)
      (Nothing :: Maybe Gio.Cancellable)
      $ Just $ \_ aresult -> do
        result :: Either GError Gio.File <- try (Gtk.fileDialogSaveFinish dialog aresult)
        case result of
          Left _ -> onCancel
          Right gfile -> do
            mPath <- #getPath gfile
            case mPath of
              Nothing -> onCancel
              Just path -> runRIO appInfo $ withPassword app $ \password ->
                onSave path password
  where
  onCancel = pure ()

fileSave :: Adw.Application -> (FilePath -> ByteString -> RIO App ()) -> RIO App ()
fileSave app onSave = do
  App {..} <- ask
  if changed then readTVarIO openedFile >>= \case
    Nothing -> showSaveDialog app onSave
    Just (OpenedFile name password) -> onSave name password
  else logInfo "File is unchanged, skipped."
