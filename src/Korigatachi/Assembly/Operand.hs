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

-- korigatachi

import Data.Text qualified as T
import Korigatachi.Types (Operand (..))
import Korigatachi.Types qualified as K

splitWord16 :: Word16 -> (Word8, Word8) -- Little-endian, (LL, HH)
splitWord16 w16 =
  let
    ll = w16 .&. 0x00FF
    hh = (w16 .&. 0xFF00) `rotateR` 8 -- move the top 8 bits to the bottom
  in
    (fromIntegral ll, fromIntegral hh)

-- operandToWord16 :: Operand -> K.Hane i i Word16
-- operandToWord16 = \case
--   Accumulator -> K.do
--     K.logErr "operandToWord16 called on valueless operand"
--     K.ixpure 0xFFFF
--   Implied -> K.do
--     K.logErr "operandToWord16 called on valueless operand"
--     K.ixpure 0xFFFF
--   Immediate w8 -> K.ixpure $ fromIntegral w8
--   IndirectX w8 -> K.ixpure $ fromIntegral w8
--   IndirectY w8 -> K.ixpure $ fromIntegral w8
--   Relative w8 -> K.ixpure $ fromIntegral w8
--   ZeroPage w8 -> K.ixpure $ fromIntegral w8
--   ZeroPageX w8 -> K.do
--     x <- K.query $ \a -> K.x . K.generalRegisters $ K.cpu a
--     K.ixpure $ fromIntegral (w8 + x)
--   ZeroPageY w8 -> K.do
--     y <- K.query $ \a -> K.y . K.generalRegisters $ K.cpu a
--     K.ixpure $ fromIntegral (w8 + y)
--   Absolute ll hh ->
--     let
--       low = fromIntegral ll
--       high = fromIntegral hh
--     in
--       K.ixpure $ high * 256 + low
--   AbsoluteX ll hh ->
--     let
--       low = fromIntegral ll
--       high = fromIntegral hh
--     in
--       K.do
--         x <- K.query $ \a -> K.x . K.generalRegisters $ K.cpu a
--         K.ixpure $ high * 256 + low + (fromIntegral x)
--   AbsoluteY ll hh ->
--     let
--       low = fromIntegral ll
--       high = fromIntegral hh
--     in
--       K.do
--         y <- K.query $ \a -> K.y . K.generalRegisters $ K.cpu a
--         K.ixpure $ high * 256 + low + (fromIntegral y)
--   Indirect ll hh ->
--     let
--       low = fromIntegral ll
--       high = fromIntegral hh
--     in
--       K.ixpure $ high * 256 + low
--   Label label -> K.do
--     romLabels <- K.query $ \a -> K.labels $ K.rom a
--     case filter (\(K.MemoryLabel tx _) -> tx == label) romLabels of
--       [] -> K.do
--         K.katteyomi ("unable to resolve label: " <> label) ""
--         K.ixpure 0xFFFF
--       ((K.MemoryLabel _ labelLocation) : _) -> K.ixpure labelLocation

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
  Label _ _ -> "Label"

isOctalDigit :: Char -> Bool
isOctalDigit c = let w = ord c in w >= 49 && w <= 55

isBinaryDigit :: Char -> Bool
isBinaryDigit c = let w = ord c in w >= 48 && w <= 49

isHexDigit :: Char -> Bool
isHexDigit c =
  let
    w = ord c
  in
    (w >= 48 && w <= 57)
      || (w >= 97 && w <= 102)
      || (w >= 65 && w <= 70)

shiftChar :: (Num a, Bits a) => Int -> Int -> Char -> a
shiftChar bits nibbles c
  | w >= 48 && w <= 57 = shiftL (fromIntegral (w - 48)) (nibbles * bits)
  | w >= 97 = shiftL (fromIntegral (w - 87)) (nibbles * bits)
  | otherwise = shiftL (fromIntegral (w - 55)) (nibbles * bits)
 where
  w = ord c

shiftNibble :: (Num a, Bits a) => Int -> Char -> a
shiftNibble = shiftChar 4

shiftTriade :: (Num a, Bits a) => Int -> Char -> a
shiftTriade = shiftChar 3

shiftBinary :: (Num a, Bits a) => Int -> Char -> a
shiftBinary = shiftChar 1

hexDigit :: Attoparsec.Parser Char
hexDigit = Attoparsec.satisfy isHexDigit Attoparsec.<?> "hexDigit"

octalDigit :: Attoparsec.Parser Char
octalDigit = Attoparsec.satisfy isOctalDigit Attoparsec.<?> "octalDigit"

binaryDigit :: Attoparsec.Parser Char
binaryDigit = Attoparsec.satisfy isBinaryDigit Attoparsec.<?> "binaryDigit"

parseBaseRepresentation :: Attoparsec.Parser K.BaseRepresentation
parseBaseRepresentation =
  (Attoparsec.string "%" *> pure K.Binary)
    <|> (Attoparsec.string "0" *> pure K.Octal)
    <|> (Attoparsec.string "$" *> pure K.Hexadecimal)
    <|> pure K.Decimal

signed :: Num a => Attoparsec.Parser a -> Attoparsec.Parser a
signed p = (negate <$> (Attoparsec.char '-' *> p)) <|> p

parseWord8 :: Attoparsec.Parser Word8
parseWord8 = fromIntegral <$> parseWord16

parseWord16 :: Attoparsec.Parser Word16
parseWord16 = signed $
  do
    baseRep <- parseBaseRepresentation
    case baseRep of
      K.Hexadecimal -> do
        nibbles <- reverse <$> Attoparsec.many1' hexDigit
        pure . getIor . foldMap Ior $ zipWith shiftNibble [0 ..] nibbles
      K.Octal -> do
        triades <- reverse <$> Attoparsec.many' octalDigit
        if null triades
          then
            pure 0 -- Gets around a single "0" being parsed as the start of an octal instead of the number zero.
          else
            pure . getIor . foldMap Ior $ zipWith shiftTriade [0 ..] triades
      K.Binary -> do
        bits <- reverse <$> Attoparsec.many1' binaryDigit
        pure . getIor . foldMap Ior $ zipWith shiftBinary [0 ..] bits
      K.Decimal -> Attoparsec.decimal

parseAccumulator :: Attoparsec.Parser Operand
parseAccumulator = (Attoparsec.char 'A') *> pure Accumulator -- You almost never need to do this.

parseImplied :: Attoparsec.Parser Operand
parseImplied = "" *> pure Implied

parseImmediate :: Attoparsec.Parser Operand
parseImmediate = (Attoparsec.char '#') *> (Immediate <$> parseWord8)

parseIndirectX :: Attoparsec.Parser Operand
parseIndirectX = do
  void $ Attoparsec.string "("
  w8 <- parseWord8
  void $ (Attoparsec.string ",x)" <|> Attoparsec.string ",X)")
  pure $ IndirectX w8

parseIndirectY :: Attoparsec.Parser Operand
parseIndirectY = do
  void $ Attoparsec.string "("
  w8 <- parseWord8
  void $ (Attoparsec.string "),y" <|> Attoparsec.string "),Y")
  pure $ IndirectY w8

parseRelative :: Attoparsec.Parser Operand
parseRelative = (Relative <$> parseWord8)

parseZeroPage :: Attoparsec.Parser Operand
parseZeroPage = (ZeroPage <$> parseWord8)

parseZeroPageX :: Attoparsec.Parser Operand
parseZeroPageX = do
  w8 <- parseWord8
  void $ (Attoparsec.string ",x" <|> Attoparsec.string ",X")
  pure $ ZeroPageX w8

parseZeroPageY :: Attoparsec.Parser Operand
parseZeroPageY = do
  w8 <- parseWord8
  void $ (Attoparsec.string ",y" <|> Attoparsec.string ",Y")
  pure $ ZeroPageY w8

parseAbsolute :: Attoparsec.Parser Operand
parseAbsolute = do
  (ll, hh) <- splitWord16 <$> parseWord16
  pure $ Absolute ll hh

parseAbsoluteX :: Attoparsec.Parser Operand
parseAbsoluteX = do
  (ll, hh) <- splitWord16 <$> parseWord16
  void $ Attoparsec.string ",x"
  pure $ AbsoluteX ll hh

parseAbsoluteY :: Attoparsec.Parser Operand
parseAbsoluteY = do
  (ll, hh) <- splitWord16 <$> parseWord16
  void $ Attoparsec.string ",y"
  pure $ AbsoluteY ll hh

parseIndirect :: Attoparsec.Parser Operand
parseIndirect = do
  void $ Attoparsec.string "("
  (ll, hh) <- splitWord16 <$> parseWord16
  void $ Attoparsec.char ')'
  pure $ Indirect ll hh

parseLabel :: Attoparsec.Parser Operand
parseLabel = (Label []) <$> Attoparsec.takeText

{-

how to parse an operand

1. parse the outside to determine which addressing mode to use (including the '$')
2. parse the number inside, word8 or word16 depending on mode
  maybe negative, then prefix, then number

-}
