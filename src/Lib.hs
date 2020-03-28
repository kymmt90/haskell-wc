{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS

data File = File {
  fileName :: String
  ,fileContents :: T.Text
  }

data Result = Result {
  numberOfLines :: Int
  , numberOfWords :: Int
  , numberOfBytes :: Int
  , resultFileName :: String
  }

instance Show Result where
  show r = printf "%7d%8d%8d %s" (numberOfLines r) (numberOfWords r) (numberOfBytes r) (resultFileName r)

wcMain :: [String] -> IO ()
wcMain inputFileNames = do
  inputFiles <-
    if inputFileNames == [] || inputFileNames == ["-"]
    then do
      readStdin
    else do
      readFiles inputFileNames

  putStrLn (unlines (map show (getWcResult inputFiles)))

readStdin :: IO [File]
readStdin = sequenceA [fmap (File "") TIO.getContents]

readFiles :: [String] -> IO [File]
readFiles fileNames = do
  contents <- sequenceA (map TIO.readFile fileNames)
  let namesAndContents = zip fileNames contents
  return $ map toFile namesAndContents
    where
      toFile (name, contents) = File name contents

getWcResult :: [File] -> [Result]
getWcResult files = map (\f -> Result {
  numberOfLines = countLines (fileContents f)
  , numberOfWords = countWords (fileContents f)
  , numberOfBytes = countBytes (fileContents f)
  , resultFileName = fileName f
  }) files

countLines :: T.Text -> Int
countLines = length . T.lines

countWords :: T.Text -> Int
countWords = length . T.words

countBytes :: T.Text -> Int
countBytes = BS.length . TE.encodeUtf8
