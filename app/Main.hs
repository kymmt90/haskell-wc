module Main where

import Data.Maybe (fromJust)
import Lib (countLines)

execute :: String -> Maybe Int
execute text = Just (countLines text)

main :: IO ()
main = do
  text <- getContents
  return (show . fromJust . execute $ text) >>= putStrLn
