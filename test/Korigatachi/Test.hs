{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Korigatachi.Test where

import Data.Attoparsec.Text qualified as Attoparsec
import Test.Tasty
import Test.Tasty.HUnit qualified as HU

import Data.Either (isLeft)
import Korigatachi.Assembly.Operand qualified as K
import Korigatachi.Types qualified as K.Types

testKorigatachi :: IO ()
testKorigatachi = defaultMain tests

tests :: TestTree
tests = testGroup "Korigatachi.Test" [parserUnitTests]

parserUnitTests :: TestTree
parserUnitTests =
  testGroup
    "Parser Unit Tests"
    [ HU.testCase "Parse Immediate Hex" $
        HU.assertEqual
          "Succeeds on \"#$FF\" as Immediate."
          (Right $ K.Types.Immediate 255)
          (Attoparsec.parseOnly K.parseImmediate "#$FF")
    , HU.testCase "Parse Immediate Signed Hex" $
        HU.assertEqual
          "Succeeds on \"#-$FF\" as Immediate."
          (Right $ K.Types.Immediate 1)
          (Attoparsec.parseOnly K.parseImmediate "-#$FF")
    , HU.testCase "Parse Immediate Octal" $
        HU.assertEqual
          "Succeeds on \"#077\" as Immediate."
          (Right $ K.Types.Immediate 63)
          (Attoparsec.parseOnly K.parseImmediate "#077")
    , HU.testCase "Parse Immediate Binary" $
        HU.assertEqual
          "Succeeds on \"#%10101010\" as Immediate."
          (Right $ K.Types.Immediate 170)
          (Attoparsec.parseOnly K.parseImmediate "#%10101010")
    , HU.testCase "Parse Immediate Bare Numeral"
        . HU.assertBool "Fails on \"FFFF\" as Immediate."
        . isLeft
        $ (Attoparsec.parseOnly K.parseImmediate "FFFF")
    , HU.testCase "Parse Immediate Missing Hash"
        . HU.assertBool "Fails on \"$FFFF\" as Immediate."
        . isLeft
        $ (Attoparsec.parseOnly K.parseImmediate "$FFFF")
    ]
