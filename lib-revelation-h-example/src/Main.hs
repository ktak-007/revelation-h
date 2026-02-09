{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Error
import qualified Revelation2
import qualified RevelationXML

import           Control.Exception (bracket_)
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (liftIO)
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitWith, ExitCode (ExitFailure), exitSuccess)
import           System.IO (stdout, hFlush, hGetEcho, hSetEcho, stdin)

-- bytestring
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

-- xml-conduit
import           Text.XML (Document(Document), Element(elementAttributes), renderLBS, def)

-- directory
import           System.Directory (doesFileExist)

-- mtl
import           Control.Monad.Except (runExceptT)

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage:\t" <> progName <> " -h | --help\n"
           <> "\t" <> progName <> " <input revelation file> <output file>"

main :: IO ()
main = do
  putStrLn "Revelation library usage example"

  (inputFileName, outputFileName) <- parseArgs

  input <- BL.readFile inputFileName
  password <- getPassword

  encodedFile <- runExceptT $ do
    decodedRawXml <- Revelation2.decrypt input password
    liftIO $ putStrLn $ "File '" <> inputFileName <> "' succesfully decoded."
    doc@(Document _ root _) <- RevelationXML.parse decodedRawXml
    liftIO $ putStrLn $ "File '" <> inputFileName <> "' successfully parsed: " <> show (elementAttributes root)
    Revelation2.encrypt (renderLBS def doc) password

  case encodedFile of
    Right enc2Out -> do
      BL.writeFile outputFileName enc2Out
      putStrLn $ "Rendered XML have been encoded and saved to '" <> outputFileName <> "'"
    Left msg -> putStrLn $ "Error: " <> msg

getPassword :: IO B.ByteString
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False BC.getLine
  putChar '\n'
  return pass
  where
  withEcho :: Bool -> IO a -> IO a
  withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

parseArgs :: IO (String, String)
parseArgs = do
  args <- getArgs
  case args of
    [option] -> do
      when (option`elem`["-h", "--help"]) $ do
        usage
        exitSuccess
      putStrLn $ "Unknown option: " <> option
      usage
      exitWith $ ExitFailure 2
    [inputFilename, outputFilename] -> do
      inputExists <- doesFileExist inputFilename
      unless (inputExists) $ do
        putStrLn $ "File '" <> inputFilename <> "' not found."
        usage
        exitWith $ ExitFailure 1
      outputExists <- doesFileExist outputFilename
      when (outputExists) $ do
        putStrLn $ "Output file '" <> outputFilename <> "' already exists!"
        usage
        exitWith $ ExitFailure 1
      return (inputFilename, outputFilename)
    _ -> do
      usage
      exitWith $ ExitFailure 2

printError :: Error.Msg -> IO ()
printError msg = putStrLn $ "Error: " <> msg

printSuccess :: a -> IO ()
printSuccess _ = putStrLn "OK"
