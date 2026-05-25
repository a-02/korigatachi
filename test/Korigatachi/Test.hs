{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Korigatachi.Test where

import Data.Attoparsec.Text qualified as Attoparsec
import Test.Tasty
import Test.Tasty.HUnit qualified as HU

import Korigatachi.Assembly.Operand qualified as K
import Data.Either (isRight, isLeft)

testKorigatachi :: IO ()
testKorigatachi = defaultMain tests

tests :: TestTree
tests = testGroup "Korigatachi.Test" [parserUnitTests]

parserUnitTests :: TestTree
parserUnitTests = testGroup "Parser Unit Tests"
  [ HU.testCase "Parse Immediate Hex" . HU.assertBool "Succeeds on \"#$FF\" as Immediate." .
      isRight $ Attoparsec.eitherResult (Attoparsec.parse K.parseImmediate "#$FF")
  , HU.testCase "Parse Immediate Signed Hex" . HU.assertBool "Succeeds on \"#-$FF\" as Immediate." .
      isRight $ Attoparsec.eitherResult (Attoparsec.parse K.parseImmediate "#$FF")
  , HU.testCase "Parse Immediate Octal" . HU.assertBool "Succeeds on \"#o77\" as Immediate." .
      isRight $ Attoparsec.eitherResult (Attoparsec.parse K.parseImmediate "#077")
  , HU.testCase "Parse Immediate Binary" . HU.assertBool "Succeeds on \"#%10101010\" as Immediate." .
      isRight $ Attoparsec.eitherResult (Attoparsec.parse K.parseImmediate "#%10101010")
  , HU.testCase "Parse Immediate Bare Numeral" . HU.assertBool "Fails on \"FFFF\" as Immediate." .
      isLeft $ Attoparsec.eitherResult (Attoparsec.parse K.parseImmediate "FFFF")
  , HU.testCase "Parse Immediate Missing Hash" . HU.assertBool "Fails on \"$FFFF\" as Immediate." .
      isLeft $ Attoparsec.eitherResult (Attoparsec.parse K.parseImmediate "$FFFF")
  
  ]
