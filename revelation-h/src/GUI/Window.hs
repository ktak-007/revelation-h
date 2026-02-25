{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.Window
  ( ApplicationId(..)
  , ApplicationProperties(..)
  , SidebarPage(..)
  , ContentPage(..)
  , runApplicationWindow
  ) where

import           GUI.Actions
import           GUI.Menu

import           Control.Monad (void, when)
import           Data.Maybe (isJust, fromJust)

-- text
import           Data.Text (Text)

-- haskell-gi-base
import           Data.GI.Base

-- gi-adwaita
import qualified GI.Adw as Adw

-- gi-gio
import qualified GI.Gio as Gio

-- gi-gtk
import qualified GI.Gtk as Gtk

newtype ApplicationId = ApplicationId Text
data SidebarPage = SidebarPage { title :: Text, content :: Maybe (IO Gtk.Widget) }
data ContentPage = ContentPage { title :: Text, subtitle :: Maybe Text, content :: Maybe (IO Gtk.Widget) }

data ApplicationProperties = ApplicationProperties
  { menu :: Menu
  , actions :: Actions
  , sidebar :: SidebarPage
  , content :: ContentPage
  }

runApplicationWindow :: ApplicationId -> (IO ApplicationProperties) -> IO ()
runApplicationWindow (ApplicationId applicationId) getAppProps = do
  app <- new Adw.Application
    [ #applicationId := applicationId
    , On #activate (getAppProps >>= activate ?self)
    ]
  void $ app.run $ Nothing

activate :: Adw.Application -> ApplicationProperties -> IO ()
activate app ApplicationProperties {..} = do
  sidebarView <- new Adw.ToolbarView []
  when (isJust sidebar.content) $
    set sidebarView [ #content :=> sidebarScrolled =<< fromJust sidebar.content ]
  sidebarView.addTopBar =<< titlebarLeft =<< createMenu menu
  sidebarPage <- new Adw.NavigationPage
    [ #child := sidebarView
    , #title := sidebar.title
    ]
  contentView <- new Adw.ToolbarView []
  when (isJust content.content) $
    set contentView [ #content :=> fromJust content.content ]
  contentView.addTopBar =<< titlebarRight content.title content.subtitle
  contentPage <- new Adw.NavigationPage
    [ #child := contentView
    , #title := content.title
    ]
  splitView <- new Adw.NavigationSplitView
    [ #sidebar := sidebarPage
    , #content := contentPage
    ]
  window <- new Adw.ApplicationWindow
    [ #application := app
    , #content :=> new Adw.ToolbarView [ #content := splitView ]
    , #defaultWidth := 600
    , #defaultHeight := 800
    ]
  initActions app actions

  window.present

titlebarLeft :: Gio.Menu -> IO Adw.HeaderBar
titlebarLeft menu = do
  headerBar <- new Adw.HeaderBar []

  headerBar.packStart =<< new Gtk.MenuButton [ #iconName := "open-menu-symbolic"
                                             , #menuModel := menu
                                             ]
  return headerBar

titlebarRight :: Text -> Maybe Text -> IO Adw.HeaderBar
titlebarRight title subtitle = do
  hb <- new Adw.HeaderBar []
  when (isJust subtitle) $
    set hb [ #titleWidget :=> new Adw.WindowTitle [ #title := title
                                                  , #subtitle := fromJust subtitle
                                                  ]
           ]
  return hb

sidebarScrolled :: Gtk.Widget -> IO Gtk.ScrolledWindow
sidebarScrolled content = new Gtk.ScrolledWindow
  [ #vexpand := True
  , #valign := Gtk.AlignFill
  , #child := content
  , #hscrollbarPolicy := Gtk.PolicyTypeNever
  , #marginStart := 4
  ]
