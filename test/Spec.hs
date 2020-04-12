{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List.Split
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Lib

prop_countLines :: String -> Bool
prop_countLines xs =
  countLines (T.pack xs) == length linesOmittedTrailingNewline
  where
    splittedByNewline = splitOn "\n" xs
    linesOmittedTrailingNewline =
      if null . last $ splittedByNewline
      then init splittedByNewline
      else splittedByNewline

prop_countWords :: String -> Bool
prop_countWords xs =
  countWords (T.pack xs) == length ([x | x <- join $ words <$> linesOmittedTrailingNewline, not (null x)])
  where
    splittedByNewline = splitOn "\n" xs
    linesOmittedTrailingNewline =
      if null . last $ splittedByNewline
      then init splittedByNewline
      else splittedByNewline

main :: IO ()
main = hspec $ do
  describe "countLines property" $ do
    prop "countLines property" prop_countLines

  describe "countWords property" $ do
    prop "countWords property" prop_countWords

  describe "countBytes" $ do
    it "counts bytes in a string" $ do
      countBytes "ab cd\tef\ngh\rjk\flm\v\n" `shouldBe` 19

    it "counts an empty string" $ do
      countBytes "\n" `shouldBe` 1

    it "does not count an empty string" $ do
      countBytes "" `shouldBe` 0

  describe "getWcResult" $ do
    context "when no file is passed" $ do
      it "produces a empty list" $ do
        (getWcResult []) `shouldBe` []

    context "when a single file is passed" $ do
      it "produces a wc result for the file" $ do
        let file = File "example.txt" "ab cd\tef\ngh\rjk\flm\v\n"
        let expected = [Result 2 6 19 "example.txt"]
        (getWcResult [file]) `shouldBe` expected

    context "when multiple files are passed" $ do
      it "produces wc results for the files" $ do
        let files =
              [
                File "example.txt" "ab cd\tef\ngh\rjk\flm\v\n"
              , File "example2.txt" "ab cd\tef\ngh\rjk\flm\v\n"
              ]
        let expected =
              [
                Result 2 6 19 "example.txt"
              , Result 2 6 19 "example2.txt"
              ]
        (getWcResult files) `shouldBe` expected

  describe "getTotal" $ do
    context "when no result is passed" $ do
      it "produces a zero result" $ do
        let expected = Result 0 0 0 "total"
        (getTotal []) `shouldBe` expected

    context "when multiple results are passed" $ do
      it "produces a result" $ do
        let results =
              [
                Result 2 6 19 "example.txt"
              , Result 3 7 20 "example2.txt"
              ]
        let expected = Result 5 13 39 "total"
        (getTotal results) `shouldBe` expected
