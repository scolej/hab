import qualified CharDecHealthTest
import qualified CharIncExpTest
import qualified GameTest
import Test.HUnit
import System.Exit

tests :: Test
tests = TestList [CharDecHealthTest.tests, CharIncExpTest.tests, GameTest.tests]

main :: IO ()
main = do
  Counts cases tried errors failures <- runTestTT tests
  if errors > 0 || failures > 0
    then exitFailure
    else exitSuccess
