{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Korigatachi.Assembly.Operand where

-- attoparsec
import Data.Attoparsec.ByteString.Char8 as Attoparsec

-- base
import Control.Applicative
import Control.Monad (void)
import Data.Bits
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Word (Word16, Word8)

-- bytestring
import Data.ByteString.Char8 qualified as BSC8

-- optics
import Optics

-- korigatachi

import Data.Text qualified as T
import Korigatachi.Control qualified as K
import Korigatachi.Model (Korigatachi)
import Korigatachi.Model qualified as K
import Korigatachi.Monad qualified as K

data Operand
  = Accumulator -- Can technically be constructed.
  | Implied -- Can't actually be constructed.
  | Immediate Word8 -- #$LL
  | IndirectX Word8 -- ($LL,x)
  | IndirectY Word8 -- ($LL),y
  | Relative Word8 -- r$LL
  | ZeroPage Word8
  | ZeroPageX Word8
  | ZeroPageY Word8
  | Absolute Word8 Word8
  | AbsoluteX Word8 Word8
  | AbsoluteY Word8 Word8
  | Indirect Word8 Word8
  | Label String -- What did you do?
  deriving (Show)

-- TODO: Make this actually find the correct address based on operand.
operandToWord16 :: Operand -> Korigatachi Word16
operandToWord16 = \case
  Accumulator -> K.do
    K.logErr "operandToWord16 called on valueless operand"
    K.ixpure 0xFFFF
  Implied -> K.do
    K.logErr "operandToWord16 called on valueless operand"
    K.ixpure 0xFFFF
  Immediate w8 -> K.ixpure $ fromIntegral w8
  IndirectX w8 -> K.ixpure $ fromIntegral w8
  IndirectY w8 -> K.ixpure $ fromIntegral w8
  Relative w8 -> K.ixpure $ fromIntegral w8
  ZeroPage w8 -> K.ixpure $ fromIntegral w8
  ZeroPageX w8 -> K.ixpure $ fromIntegral w8
  ZeroPageY w8 -> K.ixpure $ fromIntegral w8
  Absolute ll hh ->
    let
      low = fromIntegral ll
      high = fromIntegral hh
    in
      K.ixpure $ high * 256 + low
  AbsoluteX ll hh ->
    let
      low = fromIntegral ll
      high = fromIntegral hh
    in
      K.ixpure $ high * 256 + low
  AbsoluteY ll hh ->
    let
      low = fromIntegral ll
      high = fromIntegral hh
    in
      K.ixpure $ high * 256 + low
  Indirect ll hh ->
    let
      low = fromIntegral ll
      high = fromIntegral hh
    in
      K.ixpure $ high * 256 + low
  Label label -> K.do
    romLabels <- K.query $ \a -> K.labels $ K.rom a
    case filter (\(K.Label tx _) -> tx == (T.pack label)) romLabels of
      [] -> K.do
        K.katteyomi ("unable to resolve label: " <> T.pack label) ""
        K.ixpure 0xFFFF
      ((K.Label _ labelLocation) : _) -> K.ixpure labelLocation

toAddressingMode :: Operand -> K.AddressingMode
toAddressingMode = \case
  Accumulator -> K.Accumulator
  Implied -> K.Implied
  Immediate _ -> K.Immediate
  IndirectX _ -> K.IndirectX
  IndirectY _ -> K.IndirectY
  Relative _ -> K.Relative
  ZeroPage _ -> K.ZeroPage
  ZeroPageX _ -> K.ZeroPageX
  ZeroPageY _ -> K.ZeroPageY
  Absolute _ _ -> K.Absolute
  AbsoluteX _ _ -> K.AbsoluteX
  AbsoluteY _ _ -> K.AbsoluteY
  Indirect _ _ -> K.Indirect
  Label _ -> K.Implied

-- | This isn't a valid Iso. Don't use this unless you are me.
oprIso :: Iso' Operand String
oprIso = iso operandToString stringToOperand

flagsIso :: Iso' K.Flags Word8
flagsIso = iso K.flagsToWord8 K.word8ToFlags

instance IsString Operand where
  fromString = (oprIso #) -- I did this just to confuse myself.

operandToString :: Operand -> String
operandToString Accumulator = "" -- For completeness.
operandToString Implied = "" -- For completeness.
operandToString (Immediate opr) = "#$" <> (opr ^. K.hex8)
operandToString (IndirectX opr) = "($" <> (opr ^. K.hex8) <> ",x)"
operandToString (IndirectY opr) = "($" <> (opr ^. K.hex8) <> "),y"
operandToString (Relative opr) = "r$" <> (opr ^. K.hex8)
operandToString (ZeroPage opr) = "$" <> (opr ^. K.hex8)
operandToString (ZeroPageX opr) = "$" <> (opr ^. K.hex8) <> ",x"
operandToString (ZeroPageY opr) = "$" <> (opr ^. K.hex8) <> ",y"
operandToString (Absolute ll hh) =
  let
    low :: Word16
    low = fromIntegral ll
    high :: Word16
    high = fromIntegral hh
  in
    "$" <> ((high * 256 + low) ^. K.hex16) -- order of operations?
operandToString (AbsoluteX ll hh) =
  let
    low :: Word16
    low = fromIntegral ll
    high :: Word16
    high = fromIntegral hh
  in
    "$" <> ((high * 256 + low) ^. K.hex16) <> ",x" -- order of operations?
operandToString (AbsoluteY ll hh) =
  let
    low :: Word16
    low = fromIntegral ll
    high :: Word16
    high = fromIntegral hh
  in
    "$" <> ((high * 256 + low) ^. K.hex16) <> ",y" -- order of operations?
operandToString (Indirect ll hh) =
  let
    low :: Word16
    low = fromIntegral ll
    high :: Word16
    high = fromIntegral hh
  in
    "($" <> ((high * 256 + low) ^. K.hex16) <> ")" -- order of operations?
operandToString (Label lb) = lb

stringToOperand :: String -> Operand
stringToOperand operand =
  let
    isHexDigit :: Char -> Bool
    isHexDigit c =
      let
        w = ord c
      in
        (w >= 48 && w <= 57)
          || (w >= 97 && w <= 102)
          || (w >= 65 && w <= 70)

    shiftNibble :: (Num a, Bits a) => Int -> Char -> a
    shiftNibble nibbles c
      | w >= 48 && w <= 57 = shiftL (fromIntegral (w - 48)) (nibbles * 4)
      | w >= 97 = shiftL (fromIntegral (w - 87)) (nibbles * 4)
      | otherwise = shiftL (fromIntegral (w - 55)) (nibbles * 4)
     where
      w = ord c

    hexDigit :: Parser Char
    hexDigit = satisfy isHexDigit <?> "hexDigit"

    opr = BSC8.pack operand

    parseWord8 :: Parser Word8
    parseWord8 = do
      lowerHalf <- hexDigit
      upperHalf <- hexDigit
      pure $ shiftNibble 0 lowerHalf .|. shiftNibble 1 upperHalf

    parseAccumulator = "A" *> pure Accumulator -- You almost never need to do this.
    parseImplied = "" *> pure Implied

    parseImmediate = "#$" *> (Immediate <$> parseWord8)

    parseIndirectX = do
      void $ string "($"
      w8 <- parseWord8
      void $ string ",x)"
      pure $ IndirectX w8

    parseIndirectY = do
      void $ string "($"
      w8 <- parseWord8
      void $ string "),y"
      pure $ IndirectX w8

    parseRelative = "r$" *> (Relative <$> parseWord8)

    parseZeroPage = char '$' *> (ZeroPage <$> parseWord8)

    parseZeroPageX = do
      void $ char '$'
      w8 <- parseWord8
      void $ string ",x"
      pure $ ZeroPageX w8

    parseZeroPageY = do
      void $ char '$'
      w8 <- parseWord8
      void $ string ",y"
      pure $ ZeroPageY w8

    parseAbsolute = do
      void $ char '$'
      Absolute
        <$> parseWord8
        <*> parseWord8

    parseAbsoluteX = do
      void $ char '$'
      ll <- parseWord8
      hh <- parseWord8
      void $ string ",x"
      pure $ AbsoluteX ll hh

    parseAbsoluteY = do
      void $ char '$'
      ll <- parseWord8
      hh <- parseWord8
      void $ string ",y"
      pure $ AbsoluteY ll hh

    parseIndirect = do
      void $ string "($"
      ll <- parseWord8
      hh <- parseWord8
      void $ char ')'
      pure $ Indirect ll hh

    parseOperand =
      parseImmediate
        <|> parseAccumulator
        <|> parseImplied
        <|> parseIndirectX
        <|> parseIndirectY
        <|> parseRelative
        <|> parseZeroPage
        <|> parseZeroPageX
        <|> parseZeroPageY
        <|> parseAbsolute
        <|> parseAbsoluteX
        <|> parseAbsoluteY
        <|> parseIndirect
  in
    fromMaybe (Label operand) $ maybeResult (parse parseOperand opr)
