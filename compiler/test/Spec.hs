import Test.Hspec
import Spec.Parser
import Spec.Semant

main :: IO ()
main = hspec $ do
  parserTests
  typeCheckerTests
  stackFrameTests
