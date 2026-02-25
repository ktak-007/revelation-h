{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI.Menu
  ( Menu
  , Section(..)
  , createMenu
  , initActions
  ) where

import           GUI.Actions

import           Data.Foldable ( for_ )
import           Data.Text ( Text )

-- haskell-gi-base
import           Data.GI.Base

-- gi-gio
import qualified GI.Gio as Gio

type SectionName = Text
data Section = Section (Maybe SectionName) [(Text, ActionName)]
type Menu = [Section]

createMenu :: Menu -> IO Gio.Menu
createMenu menuSrc = do
  menu <- new Gio.Menu []
  for_ menuSrc $ \(Section sectionName section) -> do
    sectionMenu <- new Gio.Menu []
    mapM_ (appendItem sectionMenu) section
    menu.appendSection sectionName sectionMenu
  return menu

appendItem :: Gio.Menu -> (Text, ActionName) -> IO ()
appendItem sectionMenu (text, action) = sectionMenu.append (Just text) (Just $ "app." <> action)
