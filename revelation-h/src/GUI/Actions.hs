module GUI.Actions
  ( Action(..)
  , ActionName
  , Actions
  , initActions
  , (==>)
  , (>==)
  ) where

import           Definitions

-- rio
import           RIO hiding (Text, on)

-- text
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
            | APP_ABOUT
            deriving (Show)

type Operation = Adw.Application -> RIO App ()
type ActionName = Text
type Actions = [(ActionName, Operation)]


initActions :: Adw.Application -> Actions -> RIO App ()
initActions app actions = do
  for_ actions $ \(action, operation) ->
    createAction action operation
  where
  createAction :: ActionName -> Operation -> RIO App ()
  createAction actionName callback = do
    appInfo <- ask
    action <- Gio.simpleActionNew actionName Nothing
    on action #activate $ const $ runRIO appInfo $ callback app
    app.addAction action

showt :: Show a => a -> Text
showt o = pack $ show o

(==>) :: Text -> Action -> (Text, Text)
(==>) a b = (a, showt b)

(>==) :: Action -> Operation -> (Text, Operation)
(>==) a b = (showt a, b)
