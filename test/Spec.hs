import Test.Hspec

import Uci

main :: IO ()
main = hspec $ do
    describe "Uci.parse" $ do
        it "parses `uci` command" $ do
            Uci.parse "uci" `shouldBe` Uci.Uci

