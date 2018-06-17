import Test.Hspec
import Spec.Parser
import Spec.Semant
import Spec.Translate

main :: IO ()
main = hspec $ do
  parserTests
  semantTests
  translateTests
