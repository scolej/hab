import qualified CharDecHealthTest
import qualified CharIncExpTest
import qualified GameTest
import Test.HUnit

tests :: Test
tests = TestList [CharDecHealthTest.tests, CharIncExpTest.tests, GameTest.tests]

main :: IO ()
main = do
  runTestTT tests
  return ()
