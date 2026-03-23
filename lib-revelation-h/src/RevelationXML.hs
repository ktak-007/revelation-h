{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module RevelationXML (parse, parseEntries, render, Entry(..)) where

import qualified Error

-- base
import           Data.Data (Data)

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

-- xml-hamlet
import           Text.Hamlet.XML

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
           | Ftp        { name :: Text
                        , description :: Text
                        , updated :: Integer
                        , notes :: Text
                        , hostname :: Text
                        , port :: Text
                        , username :: Text
                        , password :: Text
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
    "ftp" -> Right $ Ftp {..}
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

  genPrefix = "generic-"
  cardPrefix = "creditcard-"
  phonePrefix = "phone-"

entryToDoc :: Entry -> [Node]
entryToDoc Generic {..} =
  [xml|
    <entry type="generic">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="generic-hostname">#{hostname}
      <field id="generic-username">#{username}
      <field id="generic-password">#{password}
  |]
entryToDoc Website {..} =
  [xml|
    <entry type="website">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="generic-url">#{url}
      <field id="generic-username">#{username}
      <field id="generic-email">#{email}
      <field id="generic-password">#{password}
  |]
entryToDoc Folder {..} =
  [xml|
    <entry type="folder">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      $forall c <- children
        ^{entryToDoc c}
  |]
entryToDoc Vnc {..} =
  [xml|
    <entry type="vnc">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="generic-hostname">#{hostname}
      <field id="generic-port">#{port}
      <field id="generic-username">#{username}
      <field id="generic-password">#{password}
  |]
entryToDoc Shell {..} =
  [xml|
    <entry type="shell">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="generic-hostname">#{hostname}
      <field id="generic-domain">#{domain}
      <field id="generic-username">#{username}
      <field id="generic-password">#{password}
  |]
entryToDoc Email {..} =
  [xml|
    <entry type="email">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="generic-email">#{email}
      <field id="generic-hostname">#{hostname}
      <field id="generic-username">#{username}
      <field id="generic-password">#{password}
  |]
entryToDoc Creditcard {..} =
  [xml|
    <entry type="creditcard">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="creditcard-cardtype">#{cardtype}
      <field id="creditcard-cardnumber">#{cardnumber}
      <field id="creditcard-expirydate">#{expirydate}
      <field id="creditcard-ccv">#{cardccv}
      <field id="generic-pin">#{pin}
  |]
entryToDoc Phone {..} =
  [xml|
    <entry type="phone">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="phone-phonenumber">#{phonenumber}
      <field id="generic-pin">#{pin}
  |]
entryToDoc Door {..} =
  [xml|
    <entry type="door">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="generic-location">#{location}
      <field id="generic-code">#{code}
   |]
entryToDoc Cryptokey {..} =
  [xml|
    <entry type="cryptokey">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="generic-hostname">#{hostname}
      <field id="generic-certificate">#{certificate}
      <field id="generic-keyfile">#{keyfile}
      <field id="generic-password">#{password}
   |]
entryToDoc Database {..} =
  [xml|
    <entry type="database">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="generic-hostname">#{hostname}
      <field id="generic-username">#{username}
      <field id="generic-password">#{password}
      <field id="generic-database">#{database}
    |]
entryToDoc Ftp {..} =
  [xml|
    <entry type="ftp">
      <name>#{name}
      <description>#{description}
      <updated>#{T.show updated}
      <notes>#{notes}
      <field id="generic-hostname">#{hostname}
      <field id="generic-port">#{port}
      <field id="generic-username">#{username}
      <field id="generic-password">#{password}
  |]

entriesToDoc :: [Entry] -> Document
entriesToDoc entries = do
  case docXml of
    [NodeElement root] -> Document (Prologue [] Nothing []) root []
    _ -> error "Can't create XML"
  where
  docXml =
    [xml|
      <revelationdata version="0.5.5" dataversion="1">
        $forall e <- entries
          ^{entryToDoc e}
    |]

render :: [Entry] -> BL.ByteString
render entries = renderLBS def $ entriesToDoc entries
