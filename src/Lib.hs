module Lib where

import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS

data Result = Result {
  numberOfLines :: Int
  , numberOfWords :: Int
  , numberOfBytes :: Int
  }

instance Show Result where
  show r = printf "%7d%8d%8d" (numberOfLines r) (numberOfWords r) (numberOfBytes r)

readStdin :: IO T.Text
readStdin = TIO.getContents

getWcResult :: T.Text -> Result
getWcResult contents = Result {
  numberOfLines = countLines contents
  , numberOfWords = countWords contents
  , numberOfBytes = countBytes contents
  }

countLines :: T.Text -> Int
countLines = length . T.lines

countWords :: T.Text -> Int
countWords = length . T.words

countBytes :: T.Text -> Int
countBytes = BS.length . TE.encodeUtf8
