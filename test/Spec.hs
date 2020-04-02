import Test.Hspec

import Uci

main :: IO ()
main = hspec $ do
    describe "Uci.parse" $ do
        it "parses `uci` command" $ do
            Uci.parse "uci" `shouldBe` Uci.Uci

        it "parses `isready` command" $ do
            Uci.parse "isready" `shouldBe` Uci.IsReady

        it "returns Unknown on unknown commands" $ do
            Uci.parse "parse this" `shouldBe` Unknown "parse this"

