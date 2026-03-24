module Definitions where

import RIO

data App = App
  { appLogFunc :: !LogFunc
  , applicationId :: Text
  , openedFile :: TVar (Maybe OpenedFile)
  , changed :: Bool
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

data OpenedFile = OpenedFile
  { name :: FilePath
  , password :: ByteString
  }
