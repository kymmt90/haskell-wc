module Main where

import Data.Maybe (fromJust)
import Lib (countLines)

execute :: String -> Maybe Int
execute text = Just (countLines text)

formatAsWc :: Int -> Maybe String
formatAsWc count = Just ("\t" ++ show count)

main :: IO ()
main = do
  result <- getContents >>= return . formatAsWc . fromJust . execute
  putStrLn $ fromJust result
