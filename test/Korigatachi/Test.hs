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
tests =
  testGroup
    "Korigatachi.Test"
    [ parserUnitTests
    , parserInternalsUnitTests
    ]

-- | Testing individual parser cases.
parserUnitTests :: TestTree
parserUnitTests =
  testGroup
    "Parser Unit Tests"
    [ parserImmediateTests
    , parseAccumulatorTests
    ]

parseAccumulatorTests :: TestTree
parseAccumulatorTests =
  testGroup
    "Parse Accumulator"
    [ HU.testCase "Parse Accumulator" $
        HU.assertEqual
          "Succeeds on \"A\" as Accumulator"
          (Right K.Types.Accumulator)
          (Attoparsec.parseOnly K.parseAccumulator "A")
          
    ]

parserImmediateTests :: TestTree
parserImmediateTests =
  testGroup
    "Parser Immediate"
    [ HU.testCase "Parse Immediate Hex" $
        HU.assertEqual
          "Succeeds on \"#$FF\" as Immediate."
          (Right $ K.Types.Immediate 255)
          (Attoparsec.parseOnly K.parseImmediate "#$FF")
    , HU.testCase "Parse Immediate Signed Hex" $
        HU.assertEqual
          "Succeeds on \"#-$FF\" as Immediate."
          (Right $ K.Types.Immediate 1)
          (Attoparsec.parseOnly K.parseImmediate "#-$FF")
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
    , HU.testCase "Fail Parse Immediate Bare Numeral"
        . HU.assertBool "Fails on \"FFFF\" as Immediate."
        . isLeft
        $ (Attoparsec.parseOnly K.parseImmediate "FFFF")
    , HU.testCase "Fail Parse Immediate Missing Hash"
        . HU.assertBool "Fails on \"$FFFF\" as Immediate."
        . isLeft
        $ (Attoparsec.parseOnly K.parseImmediate "$FFFF")
    ]


-- | Testing the internals of the parser, absent assembly syntax.
parserInternalsUnitTests :: TestTree
parserInternalsUnitTests =
  testGroup
    "Parser Internals Unit Tests"
    [ HU.testCase "Parse Word16 Hex" $
        HU.assertEqual
          "Parses \"$7FFF\" to 32767 :: Word16"
          (Right 32767)
          (Attoparsec.parseOnly K.parseWord16 "$7FFF")
    , HU.testCase "Parse Word8 Hex" $
        HU.assertEqual
          "Parses \"$6E\" to 110 :: Word8"
          (Right 110)
          (Attoparsec.parseOnly K.parseWord8 "$6E")
    , HU.testCase "Parse Word16 Octal" $
        HU.assertEqual
          "Parses \"07344\" to 3812 :: Word16"
          (Right 3812)
          (Attoparsec.parseOnly K.parseWord16 "07344")
    , HU.testCase "Parse Word8 Octal" $
        HU.assertEqual
          "Parses \"065\" to 53 :: Word8"
          (Right 53)
          (Attoparsec.parseOnly K.parseWord8 "065")
    , HU.testCase "Parse Word16 Decimal" $
        HU.assertEqual
          "Parses \"1234\" to 1234 :: Word16"
          (Right 1234)
          (Attoparsec.parseOnly K.parseWord16 "1234")
    , HU.testCase "Parse Word8 Decimal" $
        HU.assertEqual
          "Parses \"135\" to 135 :: Word8"
          (Right 135)
          (Attoparsec.parseOnly K.parseWord8 "135")
    , HU.testCase "Parse Word16 Binary" $
        HU.assertEqual
          "Parses \"%1111111100000000\" to 65280 :: Word16"
          (Right 65280)
          (Attoparsec.parseOnly K.parseWord16 "%1111111100000000")
    , HU.testCase "Parse Word8 Binary" $
        HU.assertEqual
          "Parses \"%10101010\" to 170 :: Word8"
          (Right 170)
          (Attoparsec.parseOnly K.parseWord8 "%10101010")
    ]
