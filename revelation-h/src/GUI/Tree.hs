{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module GUI.Tree ( create, TreePane(..) ) where

import RevelationXML

import           Data.Foldable ( for_, traverse_ )
import           Data.Maybe ( fromJust )
import           GHC.Stack (HasCallStack)

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

makeListStore :: [Entry] -> IO Gio.ListStore
makeListStore entries = do
  store <- Gio.listStoreNew =<< glibType @TreeNodeItem
  for_ entries $ \entry -> do
    item <- unsafeCastTo TreeNodeItem =<< new TreeNodeItem []
    gobjectSetPrivateData item ( Just entry )
    Gio.listStoreAppend store item

  return store

getChildrenFunc :: GObject.Object -> IO ( Maybe Gio.ListModel )
getChildrenFunc parent = do
  entry <- getEntry =<< unsafeCastTo TreeNodeItem parent
  case entry of
    Folder {} -> getChildModel entry
    _ -> return Nothing
  where
  getChildModel node = do
    childStore <- Gio.listStoreNew =<< glibType @TreeNodeItem
    for_ (children node) $ \child -> do
      item <- unsafeCastTo TreeNodeItem =<< new TreeNodeItem []
      gobjectSetPrivateData item ( Just child )
      Gio.listStoreAppend childStore item
    childListModel <- Gio.toListModel childStore
    return $ Just childListModel

create :: [Entry] -> (Entry -> IO ()) -> IO TreePane
create entries callback = do
  treeView <- create' entries callback
  let loadNewData newEntries = do
        -- model <- get treeView #model
        mbSelectionModel <- Gtk.listViewGetModel treeView
        whenJust mbSelectionModel $ \selectionModel -> do
          mbListModel <- Gtk.singleSelectionGetModel =<< unsafeCastTo Gtk.SingleSelection selectionModel
          whenJust mbListModel $ \listModel -> do
            rootModel <- Gtk.treeListModelGetModel =<< unsafeCastTo Gtk.TreeListModel listModel
            store <- unsafeCastTo Gio.ListStore rootModel
            Gio.listStoreRemoveAll store
            for_ newEntries $ \entry -> do
              item <- unsafeCastTo TreeNodeItem =<< new TreeNodeItem []
              gobjectSetPrivateData item ( Just entry )
              Gio.listStoreAppend store item
            return ()
          return ()
        return ()
  return TreePane { view = treeView
                  , update = loadNewData
                  }

create' :: [Entry] -> (Entry -> IO ()) -> IO Gtk.ListView
create' entries callback = do
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

    contentBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                              , #spacing := 6
                              ]
    Gtk.treeExpanderSetChild expander ( Just contentBox )

    label <- new Gtk.Label
      [ #xalign := 0
      , #marginTop := 8
      , #marginBottom := 8
      , #marginStart := 4
      , #marginEnd := 24
      ]
    Gtk.boxAppend contentBox label

    #setChild listItem (Just expander)

  on factory #bind $ \item -> do
    listItem <- unsafeCastTo Gtk.ListItem item
    mbExpander <- #getChild listItem
    expander <- case mbExpander of
        Nothing -> error "getTreeView onBind: list item has no child"
        Just expander' -> unsafeCastTo Gtk.TreeExpander expander'
    mbContentBox <- Gtk.treeExpanderGetChild expander
    label <- case mbContentBox of
      Nothing -> error "getTreeView: expected ListItem->Expander->Box"
      Just contentBox0 -> do
        contentBox <- unsafeCastTo Gtk.Box contentBox0
        mbLabel <- traverse ( unsafeCastTo Gtk.Label ) =<< Gtk.widgetGetFirstChild contentBox
        case mbLabel of
          Nothing -> error "getLayerViewWidget: expected ListItem->Expander->Box->{CheckButton,LayerLabel}"
          Just label -> return label
    entry <- getEntry listItem
    mbTreeListRow <- traverse ( unsafeCastTo Gtk.TreeListRow ) =<< Gtk.listItemGetItem listItem
    treeListRow <- case mbTreeListRow of
      Nothing -> error "newLayerView ListItem onBind: no TreeListRow"
      Just r -> return r
    Gtk.treeExpanderSetListRow expander ( Just treeListRow )
    Gtk.labelSetText label entry.name

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
      -- print entry
      callback entry

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

makeTreeViewTransparent :: Gtk.ListView -> IO ()
makeTreeViewTransparent treeView = do
  cssProvider <- new Gtk.CssProvider []
  cssClasses <- Gtk.getWidgetCssClasses treeView
  let makeTransparent cssClass = Gtk.cssProviderLoadFromString cssProvider
                               $ "." <> cssClass <> " { background-color: rgba(0, 0, 0, 0); }"
  whenJust cssClasses $ return $ traverse_ makeTransparent $ fromJust cssClasses
  styleContext <- treeView.getStyleContext
  styleContext.addProvider cssProvider $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION
  styleContext.addClass "transparent"

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
