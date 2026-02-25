{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module GUI.Actions
  ( Action(..)
  , ActionName
  , Actions
  , initActions
  , (==>)
  , (>==)
  ) where

import           Data.Foldable ( for_ )
import           Data.Text (Text, pack)

-- haskell-gi-base
import           Data.GI.Base

-- gi-adwaita
import qualified GI.Adw as Adw

-- gi-gio
import qualified GI.Gio as Gio

data Action = FILE_NEW
            | FILE_OPEN
            | FILE_SAVE
            | FILE_SAVE_AS
            | FILE_CHANGE_PASSWORD
            | APP_SAY_HELLO
            | APP_LOCK
            | APP_QUIT
            deriving (Show)

type Operation = Adw.Application -> IO ()
type ActionName = Text
type Actions = [(ActionName, Operation)]


initActions :: Adw.Application -> Actions -> IO ()
initActions app actions = do
  for_ actions $ \(action, operation) ->
    createAction action operation
  where
  createAction :: ActionName -> Operation -> IO ()
  createAction action callback = do
    action' <- Gio.simpleActionNew action Nothing
    on action' #activate $ const $ callback app
    app.addAction action'

showt :: Show a => a -> Text
showt o = pack $ show o

(==>) :: Text -> Action -> (Text, Text)
(==>) a b = (a, showt b)

(>==) :: Action -> Operation -> (Text, Operation)
(>==) a b = (showt a, b)
