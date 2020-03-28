{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "countLines" $ do
    it "counts lines in a string" $ do
      countLines "ab cd\tef\ngh\rjk\flm\v\n" `shouldBe` 2

    it "counts an empty line" $ do
      countLines "\n" `shouldBe` 1

    it "does not count lines for an empty string" $ do
      countLines "" `shouldBe` 0

  describe "countWords" $ do
    it "counts words in a string" $ do
      countWords "ab cd\tef\ngh\rjk\flm\v\n" `shouldBe` 6

    it "does not count an empty line" $ do
      countWords "\n" `shouldBe` 0

    it "does not count an empty string" $ do
      countWords "" `shouldBe` 0

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
