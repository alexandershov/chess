{-# LANGUAGE BlockArguments #-}

import Test.Hspec

import EvalSpec
import FenSpec
import MovesSpec
import UciSpec


main :: IO ()
main = hspec do
    describeUci
    describePieces
    describeFen

    describeInitialPosition
    describeMakeMove

    describeEval
