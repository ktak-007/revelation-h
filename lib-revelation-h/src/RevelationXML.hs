{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module RevelationXML (parse, parseEntries, Entry(..)) where

import qualified Error

-- bytestring
import qualified Data.ByteString.Lazy as BL

-- containers
import           Data.Map hiding (filter, map)

-- text
import           Data.Text (Text, strip, unpack)
import qualified Data.Text as T

-- xml-conduit
import           Text.XML
import           Text.XML.Cursor

-- mtl
import           Control.Monad.Except

-- base
import           Data.Data (Data)

parse :: BL.ByteString -> ExceptT Error.Msg IO Document
parse input = case parseLBS def input of
  Left err -> throwError $ show err
  Right doc@(Document _ root _) ->
    if root.elementName.nameLocalName /= "revelationdata"
    || "dataversion" `notMember` root.elementAttributes
    then throwError Error.format
    else return $ removeEmptyNodes doc

removeEmptyNodes :: Document -> Document
removeEmptyNodes (Document prologue root epilogue) = Document prologue root' epilogue
  where root' = root { elementNodes = filterElements $ filterNodes root.elementNodes}
        filterNodes :: [Node] -> [Node]
        filterNodes = filter (\case
                               NodeContent c -> strip c /= ""
                               _ -> True
                             )
        filterElements :: [Node] -> [Node]
        filterElements = map (\case
                               NodeElement e -> NodeElement $ e {
                                 elementNodes = filterElements $ filterNodes e.elementNodes
                               }
                               anyOther -> anyOther
                             )

data Entry = Generic    { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , hostname :: Text
                        , username :: Text
                        , password :: Text
                        }
            | Website   { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , url :: Text
                        , username :: Text
                        , email :: Text
                        , password :: Text
                        }
           | Folder     { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , children :: [Entry]
                        }
           | Vnc        { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , hostname :: Text
                        , port :: Text
                        , username :: Text
                        , password :: Text
                        }
           | Shell      { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , hostname :: Text
                        , domain :: Text
                        , username :: Text
                        , password :: Text
                        }
           | Email      { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , email :: Text
                        , hostname :: Text
                        , username :: Text
                        , password :: Text
                        }
           | Creditcard { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , cardtype :: Text
                        , cardnumber :: Text
                        , expirydate :: Text
                        , cardccv :: Text
                        , pin :: Text
                        }
           | Phone      { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , phonenumber :: Text
                        , pin :: Text
                        }
           | Door       { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , location :: Text
                        , code :: Text
                        }
           | Cryptokey  { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , hostname :: Text
                        , certificate :: Text
                        , keyfile :: Text
                        , password :: Text
                        }
           | Database   { name :: Text
                        , description :: Text
                        , updated :: Integer -- Timestamp
                        , notes :: Text
                        , hostname :: Text
                        , username :: Text
                        , password :: Text
                        , database :: Text
                        }
           deriving (Data, Show)

parseEntries :: BL.ByteString -> ExceptT Error.Msg IO [Entry]
parseEntries input = case parseLBS def input of
  Left err -> throwError $ show err
  Right doc@(Document _ root _) ->
    if root.elementName.nameLocalName /= "revelationdata"
    || "dataversion" `notMember` root.elementAttributes
    then throwError Error.format
    else traverse parseEntry' rootEntries
    where
    rootEntries = fromDocument doc $/ element "entry"

parseEntry' :: Cursor -> ExceptT Error.Msg IO Entry
parseEntry' node = case parseEntry node of
  Left err -> throwError $ show err
  Right entry -> return entry

parseEntry :: Cursor -> Either Error.Msg Entry
parseEntry node = do
  entryType <- requireAttribute "type"
  name <- requireName entryType
  updated <- read . unpack <$> requireElement "updated" entryType name

  case entryType of
    "folder" -> do
      children <- traverse parseEntry $ node $/ element "entry"
      Right $ Folder {..}
    "generic" -> Right $ Generic {..}
    "website" -> Right $ Website {..}
    "vnc" -> Right $ Vnc {..}
    "shell" -> Right $ Shell {..}
    "email" -> Right $ Email {..}
    "creditcard" -> Right $ Creditcard {..}
    "phone" -> Right $ Phone {..}
    "door" -> Right $ Door {..}
    "cryptokey" -> Right $ Cryptokey {..}
    "database" -> Right $ Database {..}
    etype -> Left $ Error.xmlUnknownEntryType <> " \"" <> unpack etype <> "\""
  where
  description = T.concat $ node $/ element "description" &/ content
  notes       = T.concat $ node $/ element "notes" &/ content

  hostname    = field genPrefix "hostname"
  username    = field genPrefix "username"
  password    = field genPrefix "password"
  url         = field genPrefix "url"
  email       = field genPrefix "email"
  port        = field genPrefix "port"
  domain      = field genPrefix "domain"
  cardtype    = field cardPrefix "cardtype"
  cardnumber  = field cardPrefix "cardnumber"
  expirydate  = field cardPrefix "expirydate"
  cardccv     = field cardPrefix "ccv"
  pin         = field genPrefix  "pin"
  phonenumber = field phonePrefix "phonenumber"
  location    = field genPrefix "location"
  code        = field genPrefix "code"
  certificate = field genPrefix "certificate"
  keyfile     = field genPrefix "keyfile"
  database    = field genPrefix "database"

  requireAttribute :: Text -> Either Error.Msg Text
  requireAttribute attr = case node $| laxAttribute attr of
    [x] -> Right x
    [] -> Left $ Error.xmlAttributeIsAbsent <> ": " <> unpack attr <> " in " <> show node
    _ -> Left $ Error.xmlAttributeIsNotSingle <> ": " <> unpack attr <> " in " <> show node
  requireName :: Text -> Either Error.Msg Text
  requireName etype = do
    let msgPostfix = " in entry type=\"" <> unpack etype <> "\""
    case node $/ element "name" &/ content of
      [x] -> Right x
      [] -> Left $ Error.xmlNameNotFound <> msgPostfix
      _ -> Left $ Error.xmlNameMoreThanOnce <> msgPostfix
  requireElement :: Text -> Text -> Text -> Either Error.Msg Text
  requireElement name etype ename = do
    let msgPostfix = ": " <> unpack name <> " in entry type=\"" <> unpack etype <> "\" name=\"" <> unpack ename <> "\""
    case node $/ laxElement name &/ content of
      [x] -> Right x
      [] -> Left $ Error.xmlElementIsAbsent <> msgPostfix
      _ -> Left $ Error.xmlElementIsNotSingle <> msgPostfix
  field :: Text -> Text -> Text
  field prefix fieldId = T.concat $ node $/ element "field" >=> "id" `attributeIs` (prefix <> fieldId) &/ content
  -- cardField :: Text -> Text
  -- cardField cardId = T.concat $ node $/ element "field" >=> "id" `attributeIs` (cardPrefix <> cardId) &/ content
  -- phoneField :: Text -> Text
  -- phoneField phoneId = T.concat $ node $/ element "field" >=> "id" `attributeIs` (phonePrefix <> phoneId) &/ content
  -- requireField :: Text -> Text -> Text -> Either Error.Msg Text
  -- requireField genId etype ename = do
  --   let msgPostfix = ": field with attribute " <> unpack genPrefix <> unpack genId <> " in entry type=\"" <> unpack etype <> "\" name=\"" <> unpack ename <> "\""
  --   case node $/ laxElement "field" >=> "id" `attributeIs` (genPrefix <> genId) &/ content of
  --     [x] -> Right x
  --     [] -> Left $ Error.xmlElementIsAbsent <> msgPostfix
  --     _ -> Left $ Error.xmlElementIsNotSingle <> msgPostfix
  genPrefix = "generic-"
  cardPrefix = "creditcard-"
  phonePrefix = "phone-"