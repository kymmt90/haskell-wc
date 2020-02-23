import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT $ TestList [sampleTest]
  return ()

sampleTest :: Test
sampleTest = TestList
  [ "Test 1" ~: (1 + 1 :: Int) ~?= 2
  ]
