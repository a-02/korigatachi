{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Korigatachi.Assembly.Operand where

-- attoparsec
import Data.Attoparsec.ByteString.Char8 as Attoparsec

-- base
import Control.Applicative
import Control.Monad (void)
import Data.Bits
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word8)

-- bytestring
import Data.ByteString.Char8 qualified as BSC8

-- optics
import Optics

-- korigatachi

import Data.String (IsString (fromString))
import Data.Text qualified as T
import Korigatachi.Control qualified as K
import Korigatachi.Model qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types (Korigatachi, Operand (..))
import Korigatachi.Types qualified as K

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
  ZeroPageX w8 -> K.do
    x <- K.query $ \a -> K.x . K.generalRegisters $ K.cpu a
    K.ixpure $ fromIntegral (w8 + x)
  ZeroPageY w8 -> K.do
    y <- K.query $ \a -> K.y . K.generalRegisters $ K.cpu a
    K.ixpure $ fromIntegral (w8 + y)
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
      K.do
        x <- K.query $ \a -> K.x . K.generalRegisters $ K.cpu a
        K.ixpure $ high * 256 + low + (fromIntegral x)
  AbsoluteY ll hh ->
    let
      low = fromIntegral ll
      high = fromIntegral hh
    in
      K.do
        y <- K.query $ \a -> K.y . K.generalRegisters $ K.cpu a
        K.ixpure $ high * 256 + low + (fromIntegral y)
  Indirect ll hh ->
    let
      low = fromIntegral ll
      high = fromIntegral hh
    in
      K.ixpure $ high * 256 + low
  Label label -> K.do
    romLabels <- K.query $ \a -> K.labels $ K.rom a
    case filter (\(K.MemoryLabel tx _) -> tx == (T.pack label)) romLabels of
      [] -> K.do
        K.katteyomi ("unable to resolve label: " <> T.pack label) ""
        K.ixpure 0xFFFF
      ((K.MemoryLabel _ labelLocation) : _) -> K.ixpure labelLocation

toAddressingMode :: Operand -> T.Text
toAddressingMode = \case
  Accumulator -> "Accumulator"
  Implied -> "Implied"
  Immediate _ -> "Immediate"
  IndirectX _ -> "IndirectX"
  IndirectY _ -> "IndirectY"
  Relative _ -> "Relative"
  ZeroPage _ -> "ZeroPage"
  ZeroPageX _ -> "ZeroPageX"
  ZeroPageY _ -> "ZeroPageY"
  Absolute _ _ -> "Absolute"
  AbsoluteX _ _ -> "AbsoluteX"
  AbsoluteY _ _ -> "AbsoluteY"
  Indirect _ _ -> "Indirect"
  Label _ -> "Label"

oprIso :: Iso' Operand String
oprIso = iso operandToString stringToOperand

instance IsString Operand where
  fromString = (oprIso #)

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
