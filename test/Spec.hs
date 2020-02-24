import Lib (countLines)
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList
    [
      sampleTest
    , countLinesTest
    ]
  return ()

sampleTest :: Test
sampleTest = TestList
  [ "Test 1" ~: (1 + 1 :: Int) ~?= 2
  ]

countLinesTest :: Test
countLinesTest = TestList
  [ "counts lines of a string" ~:
    countLines "ab\ncde\nf\n" ~?= 3
  , "counts an empty line" ~:
    countLines "" ~?= 0
  ]
