{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import MovesSpec
import UciSpec


main :: IO ()
main = hspec do
    describeUci
    describePieces

    describeInitialPosition
    describeMakeMove
