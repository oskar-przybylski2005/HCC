import System.FilePath (replaceExtension, takeBaseName)
import Lexer (lexer, lexe, LocatedToken)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty (testGroup, defaultMain, TestTree)
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Megaparsec.Pos (initialPos)
main :: IO ()
main = do
    cFiles <- findByExtension [".c"] "test/samples"
    let goldenTests = testGroup "Golden Tests C Files"
                        [makeGoldenTest cFile | cFile <- cFiles ]
    unitTests <- testSpec "HSpec Tests" spec

    defaultMain $ testGroup "Tests" [goldenTests, unitTests]

makeGoldenTest :: FilePath -> TestTree
makeGoldenTest cFile =
    goldenVsString
        (takeBaseName cFile)
        goldenFile
        action
  where
    goldenFile = replaceExtension cFile ".golden"
    action :: IO BS.ByteString
    action = do
        tokens <- lexer cFile
        let outputText = unlines (map show tokens)
        return (BS.pack outputText)

spec :: Spec
spec = do
  describe "Proste przypadki Lexera" $ do
    it "rozpoznaje int" $ do
      let toks = lexe (initialPos "") "int"
      length toks `shouldBe` 2

    it "obsługuje komentarze liniowe" $ do
      let code = "int x; // to jest komentarz"
      let toks = lexe (initialPos "") code
      length toks `shouldSatisfy` (> 2)
