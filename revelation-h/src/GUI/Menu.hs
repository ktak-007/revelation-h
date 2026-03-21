module GUI.Menu
  ( Menu
  , Section(..)
  , createMenu
  ) where

import           Definitions
import           GUI.Actions

-- rio
import           RIO

-- haskell-gi-base
import           Data.GI.Base

-- gi-gio
import qualified GI.Gio as Gio

type SectionName = Text
data Section = Section (Maybe SectionName) [(Text, ActionName)]
type Menu = [Section]

createMenu :: Menu -> RIO App Gio.Menu
createMenu menuSrc = do
  menu <- new Gio.Menu []
  for_ menuSrc $ \(Section sectionName section) -> do
    sectionMenu <- new Gio.Menu []
    mapM_ (appendItem sectionMenu) section
    menu.appendSection sectionName sectionMenu
  return menu

appendItem :: Gio.Menu -> (Text, ActionName) -> RIO App ()
appendItem sectionMenu (text, action) = sectionMenu.append (Just text) (Just $ "app." <> action)
