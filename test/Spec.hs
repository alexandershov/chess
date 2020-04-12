{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import PositionSpec
import UciSpec


main :: IO ()
main = hspec do
    describeUciParse
    describeUciGetResponse
    describePlay
    describeKnight
    describeBishop
    describeRook
