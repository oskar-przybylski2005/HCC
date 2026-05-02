module PostprocessorSpec (spec) where

import Common
import Postprocessor
import Test.Hspec
import Text.Megaparsec.Pos

dummyPos :: SourcePos
dummyPos = initialPos "dummy"

tok :: Token -> String -> LocatedToken
tok t = LToken t dummyPos

spec :: Spec
spec = describe "Postprocessor module" $ do
  describe "combineStringLiterals" $ do
    it "combines adjacent string literals" $ do
      let input = [tok TokenStrLit "\"hello\"", tok TokenStrLit "\"world\""]
      let expected = [tok TokenStrLit "\"helloworld\""]
      combineStringLiterals input `shouldBe` expected

    it "combines string literals separated by spaces and tabs" $ do
      let input = [tok TokenStrLit "\"hello\"", tok TokenSpace " ", tok TokenTab "\t", tok TokenStrLit "\"world\""]
      let expected = [tok TokenStrLit "\"helloworld\""]
      combineStringLiterals input `shouldBe` expected

    it "combines multiple strings" $ do
      let input = [tok TokenStrLit "\"a\"", tok TokenStrLit "\"b\"", tok TokenStrLit "\"c\""]
      let expected = [tok TokenStrLit "\"abc\""]
      combineStringLiterals input `shouldBe` expected

    it "stops combining at other tokens" $ do
      let input = [tok TokenStrLit "\"a\"", tok TokenPlus "+", tok TokenStrLit "\"b\""]
      let expected = [tok TokenStrLit "\"a\"", tok TokenPlus "+", tok TokenStrLit "\"b\""]
      combineStringLiterals input `shouldBe` expected

    it "leaves non-string tokens untouched" $ do
      let input = [tok TokenIntLit "123", tok TokenSpace " "]
      combineStringLiterals input `shouldBe` input

    it "handles empty lists" $ do
      combineStringLiterals [] `shouldBe` []

    it "handles edge case: strings separated by space but one string is empty" $ do
      let input = [tok TokenStrLit "\"\"", tok TokenSpace " ", tok TokenStrLit "\"a\""]
      let expected = [tok TokenStrLit "\"a\""]
      combineStringLiterals input `shouldBe` expected

    it "does not drop spaces if no string follows" $ do
      let input = [tok TokenStrLit "\"hello\"", tok TokenSpace " ", tok TokenSemi ";"]
      combineStringLiterals input `shouldBe` input
