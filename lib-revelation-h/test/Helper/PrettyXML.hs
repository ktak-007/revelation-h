{-# LANGUAGE OverloadedStrings #-}

module Helper.PrettyXML (prettyXML) where

import qualified Data.ByteString.Lazy as BL
import           Data.Map hiding (filter, map)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Text.XML

prettyXML :: BL.ByteString -> BL.ByteString
prettyXML bs =
  case parseLBS def bs of
    Left _    -> bs
    Right doc -> TL.encodeUtf8 . TL.fromStrict $ prettyDoc doc

prettyDoc :: Document -> Text
prettyDoc doc = renderElem 0 (documentRoot doc)

renderElem :: Int -> Element -> Text
renderElem i (Element name attrs nodes) = openTag <> children <> closeTag <> "\n"
  where
  openTag = indent i <> "<" <> nameLocalName name <> attrsText attrs <> ">"
  closeTag = "</" <> nameLocalName name <> ">"
  children = case nodes of
               [] -> ""
               _  -> "\n"
                     <> T.concat (map (renderNode (i+1)) nodes)
                     <> indent i

indent :: Int -> Text
indent n = T.replicate n "  "

attrsText :: Map Name Text -> Text
attrsText attrs = mconcat [ " " <> nameLocalName k <> "=\"" <> v <> "\""
                          | (k, v) <- toList attrs
                          ]

renderNode :: Int -> Node -> T.Text
renderNode i (NodeElement e) = renderElem i e
renderNode i (NodeContent t) = indent i <> t <> "\n"
renderNode _ _ = ""
