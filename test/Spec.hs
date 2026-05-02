import Test.Hspec
import qualified LexerSpec
import qualified LexerUnitSpec
import qualified CommonSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  LexerSpec.spec
  LexerUnitSpec.spec
  CommonSpec.spec
