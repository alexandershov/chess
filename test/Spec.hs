{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import PositionSpec
import UciSpec


main :: IO ()
main = hspec do
    describeUci
    describePieces

    describeInitialPosition
    describeMakeMove
