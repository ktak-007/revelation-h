{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module GUI.Tree ( create, TreePane(..) ) where

import           Definitions
import           RevelationXML
import qualified GUI.Icons
import           GUI.InfoPane (RenderInfo(..))

-- rio
import           RIO hiding (on, set, view)

-- base
import           Data.Maybe ( fromJust )

-- haskell-gi-base
import           Data.GI.Base
import           Data.GI.Base.GObject
import           Data.GI.Base.ShortPrelude ( whenJust )
import           Data.GI.Base.Overloading

-- gi-gtk
import qualified GI.Gtk as Gtk

-- gi-gio
import qualified GI.Gio as Gio

-- gi-gobject
import qualified GI.GObject as GObject

-- transformers
import           Control.Monad.Trans.Maybe


data TreePane = TreePane
  { view :: Gtk.ListView
  , update :: [Entry] -> IO ()
  }

-- | Custom Gtk object used to hold entry data.
--
-- These are the items that will get stored in the ListModel used by Gtk
-- to store the entry hierarchy data.
newtype TreeNodeItem = TreeNodeItem ( Gtk.ManagedPtr TreeNodeItem )

instance TypedObject TreeNodeItem  where
  glibType = registerGType TreeNodeItem

instance GObject TreeNodeItem

instance HasParentTypes TreeNodeItem
type instance ParentTypes TreeNodeItem = '[ GObject.Object ]

instance DerivedGObject TreeNodeItem where
  type GObjectParentType  TreeNodeItem = GObject.Object
  type GObjectPrivateData TreeNodeItem = Maybe Entry
  objectTypeName = "gtk-layers-TreeNodeItem"
  objectClassInit _ = return ()
  objectInstanceInit _ _ = return Nothing
  objectInterfaces = [ ]

makeListStore :: [Entry] -> RIO App Gio.ListStore
makeListStore entries = liftIO $ do
  store <- Gio.listStoreNew =<< glibType @TreeNodeItem
  for_ entries $ \entry -> do
    item <- unsafeCastTo TreeNodeItem =<< new TreeNodeItem []
    gobjectSetPrivateData item ( Just entry )
    store.append item

  return store

getChildrenFunc :: GObject.Object -> IO ( Maybe Gio.ListModel )
getChildrenFunc parent = do
  entry <- getEntry =<< unsafeCastTo TreeNodeItem parent
  when' (isFolder entry) $ getChildModel entry
  where
  getChildModel node = do
    childStore <- Gio.listStoreNew =<< glibType @TreeNodeItem
    for_ (children node) $ \child -> do
      item <- unsafeCastTo TreeNodeItem =<< new TreeNodeItem []
      gobjectSetPrivateData item ( Just child )
      childStore.append item
    childListModel <- Gio.toListModel childStore
    return childListModel
  when' cond action = if cond then Just <$> action else return Nothing

create :: [Entry] -> (RenderInfo -> IO ()) -> RIO App TreePane
create entries callback = do
  treeView <- getTreeView entries callback
  store    <- getStoreFromTreeView treeView
  let loadNewData newEntries = do
        store.removeAll
        for_ newEntries $ \entry -> do
          item <- unsafeCastTo TreeNodeItem =<< new TreeNodeItem []
          gobjectSetPrivateData item ( Just entry )
          store.append item

  return TreePane { view = treeView
                  , update = loadNewData
                  }

getStoreFromTreeView :: Gtk.ListView -> RIO App Gio.ListStore
getStoreFromTreeView treeView = liftIO $ do
  mbStore <- runMaybeT $ do
    selectionModel  <- MaybeT $ get treeView #model
    singleSelection <- MaybeT $ castTo Gtk.SingleSelection selectionModel
    listModel       <- MaybeT $ get singleSelection #model
    treeListModel   <- MaybeT $ castTo Gtk.TreeListModel listModel
    rootModel       <- liftIO $ get treeListModel #model
    store           <- MaybeT $ castTo Gio.ListStore rootModel
    pure store
  case mbStore of
    Just store -> return store
    _ -> error "getStoreFromTreeView: Store not found"

getTreeView :: [Entry] -> (RenderInfo -> IO ()) -> RIO App Gtk.ListView
getTreeView entries callback = do
  rootModel <- Gio.toListModel =<< makeListStore entries

  treeModel <- Gtk.treeListModelNew
    rootModel
    False                      -- Must not use passthrough to use TreeExpander widgets.
    False                      -- Not autoexpand on creation
    getChildrenFunc

  factory <- Gtk.signalListItemFactoryNew

  on factory #setup $ \item -> do
    listItem <- unsafeCastTo Gtk.ListItem item
    Gtk.listItemSetFocusable listItem False

    expander <- Gtk.treeExpanderNew

    Gtk.treeExpanderSetIndentForIcon  expander True
    Gtk.treeExpanderSetIndentForDepth expander True
    Gtk.treeExpanderSetHideExpander   expander False

    contentBox <- new Gtk.Box
      [ #orientation := Gtk.OrientationHorizontal
      , #spacing := 6
      ]
    expander.setChild $ Just contentBox

    icon <- new Gtk.Image
      [ #iconSize := Gtk.IconSizeNormal
      , #marginEnd := 0
      , #cssClasses := [ "invert-required" ]
      ]
    contentBox.append icon

    label <- new Gtk.Label
      [ #xalign := 0
      , #marginTop := 8
      , #marginBottom := 8
      , #marginStart := 0
      , #marginEnd := 24
      ]
    contentBox.append label

    listItem.setChild $ Just expander

  on factory #bind $ \item -> do
    listItem <- unsafeCastTo Gtk.ListItem item

    expander <- #getChild listItem >>= \case
      Nothing -> error "getTreeView onBind: list item has no child"
      Just expander' -> unsafeCastTo Gtk.TreeExpander expander'

    treeListRow <- Gtk.listItemGetItem listItem >>= traverse ( unsafeCastTo Gtk.TreeListRow ) >>= \case
      Nothing -> error "getTreeView ListItem onBind: no TreeListRow"
      Just r -> return r

    expander.setListRow $ Just treeListRow

    entry <- getEntry listItem

    mbUpdateIcon <- runMaybeT $ do
      contentBox <- MaybeT $ traverse ( unsafeCastTo Gtk.Box )   =<< Gtk.treeExpanderGetChild expander
      icon       <- MaybeT $ traverse ( unsafeCastTo Gtk.Image ) =<< Gtk.widgetGetFirstChild  contentBox
      label      <- MaybeT $ traverse ( unsafeCastTo Gtk.Label ) =<< Gtk.widgetGetNextSibling icon
      set label [ #label := entry.name ]
      return $ \expanded -> icon.setFromPaintable =<< GUI.Icons.create entry expanded

    updateIcon <- case mbUpdateIcon of
      Nothing -> error "getTreeView: expected ListItem->Expander->Box->{Image,Label}"
      Just updateIcon -> return updateIcon

    -- Set initial icon
    updateIcon False

    when (isFolder entry) $ do
      on treeListRow #notify $ pure $ updateIcon =<< get treeListRow #expanded
      pure ()

  selection <- new Gtk.SingleSelection
    [ #model := treeModel
    , #autoselect := False
    , #canUnselect := True
    ]
  set selection [ #selected := Gtk.INVALID_LIST_POSITION ]
  on selection #selectionChanged $ \_pos _n -> do
    mItem <- get selection #selectedItem
    whenJust mItem $ \item -> do
      entry <- getEntry =<< unsafeCastTo Gtk.TreeListRow item
      callback $ EntryPage entry

  treeView <- new Gtk.ListView
    [ #model := selection
    , #factory := factory
    ]
  makeTreeViewTransparent treeView

  return treeView

-- | Class for objects which wrap a 'LayerItem'.
class HasEntry a where
  getEntry :: HasCallStack => a -> IO Entry
instance HasEntry TreeNodeItem where
  getEntry item = do
    mbEntry <- gobjectGetPrivateData item
    case mbEntry of
      Nothing -> error "getLayerData: no private data"
      Just entry -> return entry
instance HasEntry Gtk.TreeListRow where
  getEntry row = do
    treeNodeItem <- treeListRowLayerItem row
    getEntry treeNodeItem
instance HasEntry Gtk.ListItem where
  getEntry listItem = do
    treeNodeItem <- treeListItemLayerItem listItem
    getEntry treeNodeItem

treeListItemLayerItem :: Gtk.ListItem -> IO TreeNodeItem
treeListItemLayerItem listItem = do
  mbListRow <- Gtk.listItemGetItem listItem
  case mbListRow of
    Nothing -> error "treeListItemLayerItem: ListItem has no item"
    Just listRow -> do
      treeListRowLayerItem =<< unsafeCastTo Gtk.TreeListRow listRow

treeListRowLayerItem :: Gtk.TreeListRow -> IO TreeNodeItem
treeListRowLayerItem listRow = do
  mbListRowItem <- Gtk.treeListRowGetItem listRow
  case mbListRowItem of
    Nothing   -> error "treeListRowLayerItem: TreeListRow has no item"
    Just item -> unsafeCastTo TreeNodeItem item

makeTreeViewTransparent :: Gtk.ListView -> RIO App ()
makeTreeViewTransparent treeView = do
  cssProvider <- new Gtk.CssProvider []
  cssClasses <- Gtk.getWidgetCssClasses treeView
  let makeTransparent cssClass = Gtk.cssProviderLoadFromString cssProvider
                               $ "." <> cssClass <> " { background-color: rgba(0, 0, 0, 0); }"
  whenJust cssClasses $ return $ traverse_ makeTransparent $ fromJust cssClasses
  styleContext <- treeView.getStyleContext
  styleContext.addProvider cssProvider $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION
  styleContext.addClass "transparent"

isFolder :: Entry -> Bool
isFolder entry = case entry of
      Folder {} -> True
      _         -> False

-- debugWidgetType :: Gtk.Widget -> IO ()
-- debugWidgetType w = do
--   gt <- gtypeFromInstance w
--   name <- gtypeName gt
--   traceIO ("[Gtk.Widget] real type = " <> name)

-- debugObjectType :: GObject.Object -> IO ()
-- debugObjectType o = do
--   gt <- gtypeFromInstance o
--   name <- gtypeName gt
--   traceIO ("[GObject.Object] real type = " <> name)
