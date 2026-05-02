module LexerUnitSpec (spec) where

import Test.Hspec
import Lexer
import Common

spec :: Spec
spec = describe "Lexer Unit Tests" $ do
    describe "isHexStart" $ do
        it "recognizes 0x" $ do
            isHexStart "0x" `shouldBe` True
            isHexStart "0x12" `shouldBe` True
        it "recognizes 0X" $ do
            isHexStart "0X" `shouldBe` True
            isHexStart "0XFF" `shouldBe` True
        it "rejects normal numbers" $ do
            isHexStart "0123" `shouldBe` False
            isHexStart "12x" `shouldBe` False
        it "handles empty string safely" $ do
            isHexStart "" `shouldBe` False

    describe "extractHex" $ do
        it "extracts hex digits" $ do
            extractHex "0x1A2b" `shouldBe` ("1A2b", "")
            extractHex "0XFFx" `shouldBe` ("FF", "x")
        it "handles missing digits" $ do
            extractHex "0x" `shouldBe` ("", "")
            extractHex "0X+" `shouldBe` ("", "+")

    describe "readLit" $ do
        it "reads until the unescaped closing char" $ do
            readLit '\'' "a'" `shouldBe` ("a'", "")
        it "handles escape sequences" $ do
            readLit '"' "a\\\"b\"c" `shouldBe` ("a\\\"b\"", "c")
            readLit '\'' "\\n'x" `shouldBe` ("\\n'", "x")
        it "handles backslash at the end of string safely" $ do
            readLit '"' "abc\\" `shouldBe` ("abc\\", "")

    describe "isValidIntSuffix" $ do
        it "validates valid suffixes" $ do
            isValidIntSuffix "u" `shouldBe` True
            isValidIntSuffix "LL" `shouldBe` True
            isValidIntSuffix "ul" `shouldBe` True
            isValidIntSuffix "Lu" `shouldBe` True
        it "rejects invalid suffixes" $ do
            isValidIntSuffix "x" `shouldBe` False
            isValidIntSuffix "ulu" `shouldBe` False

    describe "consumeIntSuffix" $ do
        it "consumes valid suffixes regardless of case" $ do
            consumeIntSuffix "ulL " `shouldBe` ("ulL", " ")
            consumeIntSuffix "LLU;" `shouldBe` ("LLU", ";")
            consumeIntSuffix "u+" `shouldBe` ("u", "+")
            consumeIntSuffix "ulLx" `shouldBe` ("ulL", "x") -- span stops at x, ulL is valid
        it "rejects invalid suffixes" $ do
            consumeIntSuffix "x " `shouldBe` ("", "x ")
            consumeIntSuffix "lul" `shouldBe` ("", "lul")

    describe "consumeExponent" $ do
        it "consumes e+digits" $ do
            consumeExponent "e+123 " `shouldBe` ("e+123", " ")
        it "consumes E-digits" $ do
            consumeExponent "E-45;" `shouldBe` ("E-45", ";")
        it "consumes e without sign" $ do
            consumeExponent "e0" `shouldBe` ("e0", "")
        it "rejects e without digits" $ do
            consumeExponent "e+" `shouldBe` ("", "e+")
            consumeExponent "E" `shouldBe` ("", "E")
        it "rejects non-exponents" $ do
            consumeExponent "f123" `shouldBe` ("", "f123")

    describe "consumeFloatSuffix" $ do
        it "consumes f, F, l, L" $ do
            consumeFloatSuffix "f" `shouldBe` ""
            consumeFloatSuffix "F " `shouldBe` " "
            consumeFloatSuffix "l;" `shouldBe` ";"
            consumeFloatSuffix "L" `shouldBe` ""
        it "leaves other chars" $ do
            consumeFloatSuffix "x" `shouldBe` "x"

    describe "character predicates" $ do
        it "identifies valid symbol chars" $ do
            isSymbolStart '_' `shouldBe` True
            isSymbolStart 'a' `shouldBe` True
            isSymbolStart '0' `shouldBe` False
            isSymbolMid '0' `shouldBe` True
            isSymbolMid '_' `shouldBe` True
            isAlpha 'Z' `shouldBe` True
            isNum '5' `shouldBe` True
            isHex 'F' `shouldBe` True
            isHex 'g' `shouldBe` False
