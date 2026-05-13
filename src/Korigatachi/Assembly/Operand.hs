{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Korigatachi.Assembly.Operand where

-- attoparsec
import Data.Attoparsec.Text qualified as Attoparsec

-- base
import Control.Applicative
import Control.Monad (void)
import Data.Bits
import Data.Char (ord)
import Data.Word (Word16, Word8)

-- bytestring

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
    case filter (\(K.MemoryLabel tx _) -> tx == label) romLabels of
      [] -> K.do
        K.katteyomi ("unable to resolve label: " <> label) ""
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
operandToString (Label lb) = T.unpack lb

stringToOperand :: String -> Operand
stringToOperand _operand = undefined

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

hexDigit :: Attoparsec.Parser Char
hexDigit = Attoparsec.satisfy isHexDigit Attoparsec.<?> "hexDigit"

parseBaseRepresentation :: Attoparsec.Parser K.BaseRepresentation
parseBaseRepresentation =
  (Attoparsec.string "%" *> pure K.Binary)
    <|> (Attoparsec.string "0" *> pure K.Octal)
    <|> (Attoparsec.string "$" *> pure K.Hexadecimal)
    <|> pure K.Decimal

-- TODO: Finish this. Add the negate sign.
parseWord8 :: Attoparsec.Parser Word8
parseWord8 = do
  baseRep <- parseBaseRepresentation
  case baseRep of
    K.Hexadecimal -> do
      lowerHalf <- hexDigit
      upperHalf <- hexDigit
      pure $ shiftNibble 0 lowerHalf .|. shiftNibble 1 upperHalf
    K.Decimal -> Attoparsec.decimal
    K.Octal -> undefined
    K.Binary -> undefined

parseAccumulator :: Attoparsec.Parser Operand
parseAccumulator = "A" *> pure Accumulator -- You almost never need to do this.

parseImplied :: Attoparsec.Parser Operand
parseImplied = "" *> pure Implied

parseImmediate :: Attoparsec.Parser Operand
parseImmediate = "#$" *> (Immediate <$> parseWord8)

parseIndirectX :: Attoparsec.Parser Operand
parseIndirectX = do
  void $ Attoparsec.string "($"
  w8 <- parseWord8
  void $ Attoparsec.string ",x)"
  pure $ IndirectX w8

parseIndirectY :: Attoparsec.Parser Operand
parseIndirectY = do
  void $ Attoparsec.string "($"
  w8 <- parseWord8
  void $ Attoparsec.string "),y"
  pure $ IndirectX w8

parseRelative :: Attoparsec.Parser Operand
parseRelative = "r$" *> (Relative <$> parseWord8)

parseZeroPage :: Attoparsec.Parser Operand
parseZeroPage = Attoparsec.char '$' *> (ZeroPage <$> parseWord8)

parseZeroPageX :: Attoparsec.Parser Operand
parseZeroPageX = do
  void $ Attoparsec.char '$'
  w8 <- parseWord8
  void $ Attoparsec.string ",x"
  pure $ ZeroPageX w8

parseZeroPageY :: Attoparsec.Parser Operand
parseZeroPageY = do
  void $ Attoparsec.char '$'
  w8 <- parseWord8
  void $ Attoparsec.string ",y"
  pure $ ZeroPageY w8

parseAbsolute :: Attoparsec.Parser Operand
parseAbsolute = do
  void $ Attoparsec.char '$'
  Absolute
    <$> parseWord8
    <*> parseWord8

parseAbsoluteX :: Attoparsec.Parser Operand
parseAbsoluteX = do
  void $ Attoparsec.char '$'
  ll <- parseWord8
  hh <- parseWord8
  void $ Attoparsec.string ",x"
  pure $ AbsoluteX ll hh

parseAbsoluteY :: Attoparsec.Parser Operand
parseAbsoluteY = do
  void $ Attoparsec.char '$'
  ll <- parseWord8
  hh <- parseWord8
  void $ Attoparsec.string ",y"
  pure $ AbsoluteY ll hh

parseIndirect :: Attoparsec.Parser Operand
parseIndirect = do
  void $ Attoparsec.string "($"
  ll <- parseWord8
  hh <- parseWord8
  void $ Attoparsec.char ')'
  pure $ Indirect ll hh

parseOperand :: Attoparsec.Parser Operand
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

addressingModeToParser :: T.Text -> Attoparsec.Parser Operand
addressingModeToParser "Accumulator" = parseAccumulator
addressingModeToParser "Implied" = parseImplied
addressingModeToParser "Indirect" = parseIndirect
addressingModeToParser "IndirectX" = parseIndirectX
addressingModeToParser "IndirectY" = parseIndirectY
addressingModeToParser "ZeroPage" = parseZeroPage
addressingModeToParser "ZeroPageX" = parseZeroPageX
addressingModeToParser "ZeroPageY" = parseZeroPageY
addressingModeToParser "Absolute" = parseAbsolute
addressingModeToParser "AbsoluteX" = parseAbsoluteX
addressingModeToParser "AbsoluteY" = parseAbsoluteY
addressingModeToParser "Relative" = parseRelative
addressingModeToParser _ = K.Label <$> Attoparsec.takeText

{-

how to parse an operand

1. parse the outside to determine which addressing mode to use (including the '$')
2. parse the number inside, word8 or word16 depending on mode
  maybe negative, then prefix, then number

-}
