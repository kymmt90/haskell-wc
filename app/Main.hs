module Main where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Lib (countLines)
import System.Environment (getArgs)

count :: String -> Maybe Int
count text = Just (countLines text)

formatAsWc :: Int -> Maybe String
formatAsWc result = Just ("\t" ++ show result)

main :: IO ()
main = do
  files <- getArgs

  case files of
    [] -> do
      readFromStdin
    _ -> do
      readFromFiles files

  where
    readFromFiles :: [String] -> IO ()
    readFromFiles files =
      forM_ files $ \f -> do
        result <- readFile f >>= return . formatAsWc . fromJust . count
        putStrLn $ fromJust result ++ " " ++ f

    readFromStdin :: IO ()
    readFromStdin = do
      result <- getContents >>= return . formatAsWc . fromJust . count
      putStrLn $ fromJust result
