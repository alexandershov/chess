{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import PositionSpec
import UciSpec


main :: IO ()
main = hspec do
    describeUci

    describeWhitePawn
    describeBlackPawn
    describeKnight
    describeBishop
    describeRook
    describeQueen
    describeKing

    describeInitialPosition
    describeMakeMove


