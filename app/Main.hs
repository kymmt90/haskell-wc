module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  getArgs >>= wcMain
