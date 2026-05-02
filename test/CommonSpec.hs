module CommonSpec (spec) where

import Test.Hspec
import Text.Megaparsec.Pos
import Common

spec :: Spec
spec = describe "Common module" $ do
    describe "advPos" $ do
        it "advances column by 1 for normal characters" $ do
            let pos = initialPos "test"
            let newPos = advPos pos 'a'
            unPos (sourceColumn newPos) `shouldBe` 2
            unPos (sourceLine newPos) `shouldBe` 1

        it "advances column by 4 for tabs" $ do
            let pos = initialPos "test"
            let newPos = advPos pos '\t'
            unPos (sourceColumn newPos) `shouldBe` 5
            unPos (sourceLine newPos) `shouldBe` 1

        it "resets column and advances line for newlines" $ do
            let pos = initialPos "test"
            let pos1 = advPos pos 'a'
            let newPos = advPos pos1 '\n'
            unPos (sourceColumn newPos) `shouldBe` 1
            unPos (sourceLine newPos) `shouldBe` 2

    describe "advStr" $ do
        it "advances position over a string" $ do
            let pos = initialPos "test"
            let newPos = advStr pos "abc\tdef\nx"
            unPos (sourceColumn newPos) `shouldBe` 2
            unPos (sourceLine newPos) `shouldBe` 2

        it "handles empty string" $ do
            let pos = initialPos "test"
            let newPos = advStr pos ""
            newPos `shouldBe` pos
