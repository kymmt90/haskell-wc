module Lib
    ( countLines
    ) where

countLines :: String -> Int
countLines = length . lines
