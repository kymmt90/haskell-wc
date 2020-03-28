{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  inputFileNames <- getArgs
  inputContents <- if inputFileNames == [] || inputFileNames == ["-"] then readStdin else return ""
  putStrLn . show $ getWcResult inputContents
