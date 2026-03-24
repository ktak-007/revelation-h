module Definitions where

import RIO

data App = App
  { appLogFunc :: !LogFunc
  , applicationId :: Text
  , openedFile :: TVar (Maybe OpenedFile)
  }

data OpenedFile = OpenedFile
  { name :: FilePath
  , password :: ByteString
  }
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
