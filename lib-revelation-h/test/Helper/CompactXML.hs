module Helper.CompactXML (compactXML) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import           Text.XML

compactXML :: BL.ByteString -> BL.ByteString
compactXML bs = case parseLBS def bs of
  Left _    -> bs
  Right doc -> renderLBS def (stripFormatting doc)

stripFormatting :: Document -> Document
stripFormatting doc = doc { documentRoot = goElem (documentRoot doc) }

goElem :: Element -> Element
goElem el = el { elementNodes = filter (not . isFormatting) (map goNode (elementNodes el)) }

isFormatting :: Node -> Bool
isFormatting (NodeContent t) = T.all (`elem` [' ', '\n', '\r', '\t']) t
isFormatting _ = False

goNode :: Node -> Node
goNode (NodeElement e) = NodeElement (goElem e)
goNode n               = n
