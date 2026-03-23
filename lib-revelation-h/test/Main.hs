{-# LANGUAGE  OverloadedStrings #-}

module Main (main) where

import qualified RevelationXML
import           Helper.CompactXML
import           Helper.PrettyXML

import qualified Data.ByteString.Lazy as BL
import Control.Monad.Except (runExceptT)
import Test.HUnit (assertEqual)


main :: IO ()
main = do
  xmlContent <- compactXML <$> BL.readFile "test/example.xml"

  -- Parse XML
  entriesOrErr <- runExceptT $ RevelationXML.parseEntries xmlContent
  let entries = either error id entriesOrErr

  -- Render entries back to XML
  let rendered = RevelationXML.render entries

  -- Compare final renderings
  assertEqual "Round-trip XML rendering should be identical"
    (prettyXML xmlContent)
    (prettyXML rendered)

  putStrLn "All tests passed!"
