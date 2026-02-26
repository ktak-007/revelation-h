{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

module GUI.InfoPane
  ( InfoPane(..)
  , create
  ) where

import           RevelationXML (Entry(..))

-- gi-adwaita
import qualified GI.Adw as Adw

-- gi-glib
import qualified GI.GLib as GLib

-- gi-gtk
import qualified GI.Gtk as Gtk

-- haskell-gi-base
import           Data.GI.Base
import           Data.GI.Base.Utils (whenJust)

-- text
import           Data.Text (Text)
import qualified Data.Text as T

-- Hclip
import           System.Hclip (setClipboard)

-- base
import           Control.Monad (unless)
import           Data.Data (Data, toConstr, showConstr)
import           Data.Int (Int64)

data InfoPane = InfoPane
  { view   :: Gtk.CenterBox
  , render :: Entry -> IO ()
  }

data RenderFunctions = RenderFunctions
  { addLink :: Text -> Text -> IO ()
  , addField :: Text -> Text -> IO ()
  , addHidden :: Text -> Text -> IO ()
  , setNotes :: Text -> IO ()
  , setUpdated :: Entry -> IO ()
  }

data RenderParams = RenderParams Entry RenderFunctions

create :: IO InfoPane
create = do
  content <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 6
    , #halign := Gtk.AlignCenter
    , #valign := Gtk.AlignCenter
    , #marginTop := 24
    , #marginBottom := 24
    , #marginStart := 24
    , #marginEnd := 24
    ]

  clamp <- new Adw.Clamp
    [ #maximumSize := 700
    , #tighteningThreshold := 500
    , #child := content
    ]

  outer <- new Gtk.CenterBox
    [ #hexpand := True
    , #vexpand := True
    , #centerWidget := clamp
    ]

  -- Type field
  typeLabel <- new Gtk.Label [ #xalign := 0.5 ]
  Gtk.widgetAddCssClass typeLabel "dim-label"
  #append content typeLabel
  let setType text = set typeLabel [ #label := text ]

  -- Header
  headerLabel <- new Gtk.Label
    [ #label := "Hello, World!"
    , #useMarkup := True
    , #xalign := 0.5
    ]
  Gtk.widgetAddCssClass headerLabel "title-1"
  #append content headerLabel
  let setHeader text = set headerLabel [ #label := text ]

  -- Description field
  descriptionLabel <- new Gtk.Label [ #xalign := 0.5 ]
  Gtk.widgetAddCssClass descriptionLabel "dim-label"
  #append content descriptionLabel
  let setDescription text = unless (T.null text) $ do
        #setLabel descriptionLabel text
        set descriptionLabel [ #visible := True ]
  let cleanDescription = Gtk.widgetSetVisible descriptionLabel False

  -- Group of fields
  group <- new Adw.PreferencesGroup [ #marginTop := 12 ]
  #append content group

  let addField name value = do
        valueLabel <- new Gtk.Label
          [ #label := value
          , #valign := Gtk.AlignCenter
          ]
        Gtk.widgetAddCssClass valueLabel "dim-label"

        row <- new Adw.ActionRow [ #title := name ]
        Adw.actionRowAddSuffix row valueLabel
        Adw.preferencesGroupAdd group row

  let addHidden name value = do
        box <- new Gtk.Box
          [ #orientation := Gtk.OrientationHorizontal
          , #spacing := 6
          , #valign := Gtk.AlignCenter
          ]
        valueLabel <- new Gtk.Label
          [ #label := "xxxxxxxxxxxxxxxxxxxx"
          , #valign := Gtk.AlignCenter
          ]
        Gtk.widgetAddCssClass valueLabel "dim-label"
        #append box valueLabel
        button <- new Gtk.Button
          [ #iconName := "edit-copy-symbolic"
          , #tooltipText := "Copy to clipboard"
          , #hasFrame := False
          ]
        Gtk.widgetAddCssClass button "flat"
        on button #clicked $ do
          setClipboard $ T.unpack value
          set button [ #iconName := "object-select-symbolic" ]
          GLib.timeoutAdd GLib.PRIORITY_DEFAULT 2000 $ do
            set button [ #iconName := "edit-copy-symbolic" ]
            return False
          return ()
        #append box button
        row <- new Adw.ActionRow [ #title := name ]
        Adw.actionRowAddSuffix row box
        Adw.preferencesGroupAdd group row

  let addLink name url = do
        link <- new Gtk.Label
          [ #useMarkup := True
          , #label := "<a href=\"" <> url <> "\">" <> url <> "</a>"
          , #valign := Gtk.AlignCenter
          ]
        row <- new Adw.ActionRow [ #title := name ]
        Adw.actionRowAddSuffix row link
        Adw.preferencesGroupAdd group row

  let cleanGroup = do
        let loop mbNext = do
              whenJust mbNext $ \next -> do
                mbActionRow <- castTo Adw.ActionRow next
                mbNext' <- Gtk.widgetGetNextSibling next
                case mbActionRow of
                  Just actionRow -> Adw.preferencesGroupRemove group actionRow
                  _              -> loop =<< Gtk.widgetGetFirstChild next
                loop mbNext'
        loop =<< Gtk.widgetGetFirstChild group

  -- Notes field
  notesCardFrame <- new Gtk.Frame [ #vexpand := False ]
  Gtk.widgetAddCssClass notesCardFrame "card"

  -- Hide and show notes card frame in slider
  notesCardRevealer <- new Gtk.Revealer
    [ #child := notesCardFrame
    , #transitionType := Gtk.RevealerTransitionTypeSlideDown
    , #transitionDuration := 500
    , #revealChild := False
    ]
  #append content notesCardRevealer

  notesHeader <- new Gtk.Label
    [ #label := "Notes"
    , #halign := Gtk.AlignStart
    , #marginBottom := 6
    ]
  -- Gtk.widgetAddCssClass notesHeader "heading"
  -- Gtk.widgetAddCssClass notesHeader "title-3"
  -- Gtk.widgetAddCssClass notesHeader "dim-label"

  notesTextView <- new Gtk.TextView
    [ #wrapMode := Gtk.WrapModeWord
    , #vexpand := True
    , #editable := False
    ]

  notesScroll <- new Gtk.ScrolledWindow
    [ #child := notesTextView
    , #vexpand := True
    , #minContentHeight := 120
    ]
  Gtk.widgetAddCssClass notesScroll "dim-label"

  notesBox <- new Gtk.Box
    [ #orientation :=  Gtk.OrientationVertical
    , #spacing := 12
    , #marginTop := 12
    , #marginBottom := 12
    , #marginStart := 12
    , #marginEnd := 12
    ]
  #append notesBox notesHeader
  #append notesBox notesScroll

  set notesCardFrame [ #child := notesBox ]

  let setNotes notesText = do
        buffer <- Gtk.textViewGetBuffer notesTextView
        Gtk.textBufferSetText buffer notesText (-1)
        Gtk.revealerSetRevealChild notesCardRevealer True
  let cleanNotes = do
        Gtk.revealerSetRevealChild notesCardRevealer False

  -- Updated field
  updatedLabel <- new Gtk.Label
    [ #halign := Gtk.AlignCenter
    , #visible := False
    , #marginTop := 12
    ]
  Gtk.widgetAddCssClass updatedLabel "dim-label"
  #append content updatedLabel
  let setUpdated entry = do
        dateStr <- formatDate entry.updated
        agoStr <- timeAgo entry.updated
        set updatedLabel [ #label := agoStr ]
        Gtk.widgetSetTooltipText updatedLabel $ Just dateStr
        set updatedLabel [ #visible := True ]
  let cleanUpdated = set updatedLabel [ #visible := False ]

  -- Generic helpers
  let cleanInfoPane = do
        -- -- плавный fade-out карточки (криво работает)
        -- set outer [ #opacity := 1.0 ]
        -- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 16 $ do
        --   current <- get outer #opacity
        --   let next = max 0.0 (current - 0.05)
        --   set outer [ #opacity := next ]
        --   pure (next >= 0.0)

        cleanGroup
        cleanDescription
        cleanNotes
        cleanUpdated

  let renderBox entry = do
        cleanInfoPane

        setHeader entry.name
        setType $ getType entry
        setDescription entry.description

        -- renderEntry' entry addLink addField setNotes setUpdated
        renderEntry $ RenderParams entry
                    $ RenderFunctions addLink addField addHidden setNotes setUpdated

        case entry of
          Folder {} -> return ()
          _ -> do
            entry.notes ? setNotes
            setUpdated entry

        -- плавный fade-in карточки
        -- set outer [ #opacity := 0.0 ]
        -- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 16 $ do
        --   current <- get outer #opacity
        --   let next = min 1.0 (current + 0.05)
        --   set outer [ #opacity := next ]
        --   pure (next < 1.0)
        -- return ()

  return InfoPane { view = outer
                  , render = renderBox
                  }

renderEntry :: RenderParams -> IO ()
renderEntry (RenderParams entry RenderFunctions {..}) = case entry of
  Folder {} -> return ()
  Generic {..} -> do
    hostname ? field "Hostname"
    username ? field "Username"
    password ? hidden "Password"
  Website {..} -> do
    url ? addLink "Url"
    username ? field "Username"
    email ? field "Email"
    password ? hidden "Password"
  Vnc {..} -> do
    hostname ? field "Hostname"
    port ? field "Port"
    username ? field "Username"
    password ? hidden "Password"
  Shell {..} -> do
    hostname ? field "Hostname"
    domain ? field "Domain"
    username ? field "Username"
    password ? hidden "Password"
  Email {..} -> do
    email ? field "Email"
    hostname ? field "Hostname"
    username ? field "Username"
    password ? hidden "Password"
  Creditcard {..} -> do
    cardtype ? field "Card type"
    cardnumber ? field "Card number"
    expirydate ? field "Expiration date"
    cardccv ? field "CCV number"
    pin ? hidden "PIN"
  Phone {..} -> do
    phonenumber ? field "Phone number"
    pin ? hidden "PIN"
  Door {..} -> do
    location ? field "Location"
    code ? hidden "Code"
  Cryptokey {..} -> do
    hostname ? field "Hostame"
    certificate ? field "Certificate"
    keyfile ? field "Key file"
    password ? hidden "Password"
  Database {..} -> do
    hostname ? field "Hostname"
    username ? field "Username"
    password ? hidden "Password"
    database ? field "Database"
  where
  field = addField
  hidden = addHidden

(?) :: Applicative f => Text -> (Text -> f ()) -> f ()
t ? f = unless (T.null t) (f t)

-- withDefaults :: (IsDescendantOf Gtk.Widget w, GObject w) => w -> IO w
-- withDefaults widget = do
--   widget' <- Gtk.toWidget widget
--   set widget'
--     [ #hexpand      := False
--     , #vexpand      := False
--     , #halign       := Gtk.AlignCenter
--     , #valign       := Gtk.AlignCenter
--     ]
--   return widget

getType :: Data a => a -> Text
getType = T.pack . showConstr . toConstr

formatDate :: Integer -> IO Text
formatDate ts = do
  mDt <- GLib.dateTimeNewFromUnixLocal (fromIntegral ts :: Int64)
  case mDt of
    Nothing -> return "Invalid date"
    Just dt -> do
      mStr <- GLib.dateTimeFormat dt "%x %X"
      return $ maybe "Invalid format" id mStr

timeAgo :: Integer -> IO Text
timeAgo ts = do
  mbNow  <- GLib.dateTimeNewNowLocal
  mbPast <- GLib.dateTimeNewFromUnixLocal $ fromIntegral ts

  case (mbNow, mbPast) of
    (Just now, Just past) -> do
      diff <- GLib.dateTimeDifference now past
      let seconds = diff `div` 1000_000  -- microseconds → seconds
      return $ humanize seconds
    _ -> return ""

humanize :: Int64 -> Text
humanize seconds = "Updated " <> units <> " ago"
  where
    units | seconds < 60 = T.show seconds <> " seconds"
          | minutes < 60 = T.show minutes <> " minutes"
          | hours   < 24 = T.show hours   <> " hours"
          | days    < 30 = T.show days    <> " days"
          | months  < 12 = T.show months  <> " months"
          | otherwise    = T.show years   <> " years"
    minutes = seconds `div` 60
    hours   = minutes `div` 60
    days    = hours   `div` 24
    months  = days    `div` 30
    years   = days    `div` 365

-- debugWidgetType :: Gtk.Widget -> IO ()
-- debugWidgetType w = do
--   gt <- gtypeFromInstance w
--   name <- gtypeName gt
--   traceIO ("[Gtk.Widget] real type = " <> name)
