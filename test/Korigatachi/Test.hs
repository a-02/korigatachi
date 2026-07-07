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
    [ parseImmediateTests
    , parseAccumulatorTests
    , parseImpliedTests
    , parseIndirectXTests
    , parseIndirectYTests
    , parseRelativeTests
    , parseZeroPageTests
    , parseZeroPageXTests
    , parseZeroPageYTests
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

parseImpliedTests :: TestTree
parseImpliedTests =
  testGroup
    "Parse Implied"
    [ HU.testCase "Parse Implied" $
        HU.assertEqual
          "Succeeds on \"\" as Implied"
          (Right K.Types.Implied)
          (Attoparsec.parseOnly K.parseImplied "")
          
    ]


parseImmediateTests :: TestTree
parseImmediateTests =
  testGroup
    "Parse Immediate"
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

parseIndirectXTests :: TestTree
parseIndirectXTests =
  testGroup
    "Parser IndirectX"
    [ HU.testCase "Parse IndirectX Hex" $
        HU.assertEqual
          "Succeeds on \"($FF,x)\" as IndirectX."
          (Right $ K.Types.IndirectX 255)
          (Attoparsec.parseOnly K.parseIndirectX "($FF,x)")
    , HU.testCase "Parse IndirectX Signed Hex" $
        HU.assertEqual
          "Succeeds on \"(-$FF,x)\" as IndirectX."
          (Right $ K.Types.IndirectX 1)
          (Attoparsec.parseOnly K.parseIndirectX "(-$FF,x)")
    , HU.testCase "Parse IndirectX Octal" $
        HU.assertEqual
          "Succeeds on \"(077,x)\" as IndirectX."
          (Right $ K.Types.IndirectX 63)
          (Attoparsec.parseOnly K.parseIndirectX "(077,x)")
    , HU.testCase "Parse IndirectX Binary" $
        HU.assertEqual
          "Succeeds on \"(%10101010,x)\" as IndirectX."
          (Right $ K.Types.IndirectX 170)
          (Attoparsec.parseOnly K.parseIndirectX "(%10101010,x)")
    , HU.testCase "Fail Parse IndirectX Bare Numeral"
        . HU.assertBool "Fails on \"FFFF,x\" as IndirectX."
        . isLeft
        $ (Attoparsec.parseOnly K.parseIndirectX "FFFF,x")
    ]
    
parseIndirectYTests :: TestTree
parseIndirectYTests =
  testGroup
    "Parser IndirectY"
    [ HU.testCase "Parse IndirectY Hex" $
        HU.assertEqual
          "Succeeds on \"($FF),y\" as IndirectY."
          (Right $ K.Types.IndirectY 255)
          (Attoparsec.parseOnly K.parseIndirectY "($FF),y")
    , HU.testCase "Parse IndirectY Signed Hex" $
        HU.assertEqual
          "Succeeds on \"(-$FF),y\" as IndirectY."
          (Right $ K.Types.IndirectY 1)
          (Attoparsec.parseOnly K.parseIndirectY "(-$FF),y")
    , HU.testCase "Parse IndirectY Octal" $
        HU.assertEqual
          "Succeeds on \"(077),y\" as IndirectY."
          (Right $ K.Types.IndirectY 63)
          (Attoparsec.parseOnly K.parseIndirectY "(077),y")
    , HU.testCase "Parse IndirectY Binary" $
        HU.assertEqual
          "Succeeds on \"(%10101010),y\" as IndirectY."
          (Right $ K.Types.IndirectY 170)
          (Attoparsec.parseOnly K.parseIndirectY "(%10101010),y")
    , HU.testCase "Fail Parse IndirectY Wrong Register"
        . HU.assertBool "Fails on \"($FFFF),x\" as IndirectY."
        . isLeft
        $ (Attoparsec.parseOnly K.parseIndirectY "($FFFF),x")
    ]

parseRelativeTests :: TestTree
parseRelativeTests =
  testGroup
    "Parse Relative"
    [ HU.testCase "Parse Relative" $
        HU.assertEqual
          "Succeeds on \"$02\" as Relative"
          (Right $ K.Types.Relative 2)
          (Attoparsec.parseOnly K.parseRelative "$02")
    ]

parseZeroPageTests :: TestTree
parseZeroPageTests =
  testGroup
    "Parse ZeroPage"
    [ HU.testCase "Parse ZeroPage" $
        HU.assertEqual
          "Succeeds on \"$02\" as ZeroPage"
          (Right $ K.Types.ZeroPage 2)
          (Attoparsec.parseOnly K.parseZeroPage "$02")
    ]

parseZeroPageXTests :: TestTree
parseZeroPageXTests =
  testGroup
    "Parse ZeroPageX"
    [ HU.testCase "Parse ZeroPageX" $
        HU.assertEqual
          "Succeeds on \"$02,x\" as ZeroPageX"
          (Right $ K.Types.ZeroPageX 2)
          (Attoparsec.parseOnly K.parseZeroPageX "$02,x")
    , HU.testCase "Parse ZeroPageX Capitalized" $
        HU.assertEqual
          "Succeeds on \"$03,X\" as ZeroPageX"
          (Right $ K.Types.ZeroPageX 3)
          (Attoparsec.parseOnly K.parseZeroPageX "$03,X")
    ]

parseZeroPageYTests :: TestTree
parseZeroPageYTests =
  testGroup
    "Parse ZeroPageY"
    [ HU.testCase "Parse ZeroPageY" $
        HU.assertEqual
          "Succeeds on \"$02,y\" as ZeroPageY"
          (Right $ K.Types.ZeroPageY 2)
          (Attoparsec.parseOnly K.parseZeroPageY "$02,y")
    , HU.testCase "Parse ZeroPageY Capitalized" $
        HU.assertEqual
          "Succeeds on \"$03,Y\" as ZeroPageY"
          (Right $ K.Types.ZeroPageY 3)
          (Attoparsec.parseOnly K.parseZeroPageY "$03,Y")
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
    , HU.testCase "Parse Word8 Zero" $
        HU.assertEqual
          "Parses \"0\" to 0 :: Word8"
          (Right 0)
          (Attoparsec.parseOnly K.parseWord8 "0")
    ]
