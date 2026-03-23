module Definitions where

import RIO

data App = App
  { applicationId :: Text
  , openedFile :: TVar (Maybe OpenedFile)
  }

data OpenedFile = OpenedFile
  { name :: FilePath
  , password :: ByteString
  }
