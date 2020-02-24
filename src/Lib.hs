module Lib
    ( countLines
    ) where

import qualified Data.Text as T

countLines :: String -> Int
countLines = length . T.lines . T.pack
