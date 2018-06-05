import Test.Hspec
import Spec.Parser
import Spec.TypeChecker

main :: IO ()
main = hspec $ do
  parserTests
  typeCheckerTests
