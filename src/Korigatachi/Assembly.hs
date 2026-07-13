{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}

{- HLINT ignore "Use $>" -}

module Korigatachi.Assembly where

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Bits (Bits ((.&.)))
import Data.Text qualified as T
import Data.Word (Word16)
import Korigatachi.Assembly.Operand
import Korigatachi.Assembly.ReadWrite
import Korigatachi.Control qualified as K
import Korigatachi.Model qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types (Korigatachi, Operand (..))
import Korigatachi.Types qualified as K
import Optics
import Prelude hiding (read, and)

{- | Assembly instructions are indented to semantically
seperate them from labels and other not-assembly things.
This is hard-coded to two spaces because it looks nice.
-}
spacing :: T.Text
spacing = "  "

{- | The start of a valid Atari 2600 asm file.
Korigatachi is meant to not only assemble Atari machine code
itself, but also spit out assembly that is readable by the dasm
8-bit assembler.
-}
preamble :: Korigatachi ()
preamble = K.do
  K.codeGen "  processor 6502"
  K.codeGen "  include vcs.h"

{- | Label a part of the ROM.
You can use this to refer to different sections of the program.
-}
label :: T.Text -> Korigatachi ()
label labelText = K.do
  labelByte <- K.query $ \a -> a.rom.focus
  K.modify $ #rom % #labels %~ (\ls -> K.MemoryLabel labelText labelByte : ls)
  K.codeGen labelText

resolveLabel :: T.Text -> Korigatachi T.Text
resolveLabel labelText = K.do
  atari <- K.get 
  let
    labels = atari.rom.labels
    shortCircuitFilter _ _ [] = 0
    shortCircuitFilter f predicate (x:xs) =
      if predicate x then f x else shortCircuitFilter f predicate xs
  pure . T.show $ shortCircuitFilter K.labelByte (\(K.MemoryLabel memoryLabel _) -> memoryLabel == labelText) labels

{- | Move where the assembler writes bytes to.
org 0xFFFC will place the "focus" at the 2nd to last byte
in ROM.
-}
org :: Word16 -> Korigatachi ()
org w16 = K.do
  K.modify $ #rom % #focus .~ (w16 .&. 0x0FFF)
  K.codeGen (spacing <> "org $" <> (T.pack $ w16 ^. K.hex16))

-- | Place a 16-bit word in the ROM. Little-endian.
word :: Word16 -> Korigatachi ()
word w16 = K.do
  let
    (ll, hh) = splitWord16 w16
  assembleROM ll
  assembleROM hh
  K.codeGen (spacing <> ".word $" <> (T.pack $ w16 ^. K.hex16))

adc :: T.Text -> Korigatachi ()
adc oprText = K.do
  K.codeGen (spacing <> "adc " <> oprText)
  let
    parseASL =
      parseImmediate <|>
      parseZeroPage <|>
      parseZeroPageX <|>
      parseAbsolute <|>
      parseAbsoluteX <|>
      parseAbsoluteY <|>
      parseIndirectX <|>
      parseIndirectY <|>
      parseLabel
  case Attoparsec.parseOnly parseASL oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) ->
      void $ traverse assembleROM [0x69, w8]
    Right (ZeroPage w8) ->
      void $ traverse assembleROM [0x65, w8]
    Right (ZeroPageX w8) ->
      void $ traverse assembleROM [0x75, w8]
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0x6d, ll, hh]
    Right (AbsoluteX ll hh) ->
      void $ traverse assembleROM [0x7d, ll, hh]
    Right (AbsoluteY ll hh) ->
      void $ traverse assembleROM [0x7d, ll, hh]
    Right (IndirectX w8) ->
      void $ traverse assembleROM [0x61, w8]
    Right (IndirectY w8) ->
      void $ traverse assembleROM [0x71, w8]
    Right (Label text) -> K.do
      res <- resolveLabel text
      adc res
    _ -> K.log K.Warn ("Invalid operand for ADC: " <> oprText)

and :: T.Text -> Korigatachi ()
and oprText = K.do
  K.codeGen (spacing <> "and " <> oprText)
  let
    parseAND =
      parseImmediate <|>
      parseZeroPage <|>
      parseZeroPageX <|>
      parseAbsolute <|>
      parseAbsoluteX <|>
      parseAbsoluteY <|>
      parseIndirectX <|>
      parseIndirectY <|>
      parseLabel
  case Attoparsec.parseOnly parseAND oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) ->
      void $ traverse assembleROM [0x29, w8]
    Right (ZeroPage w8) ->
      void $ traverse assembleROM [0x25, w8]
    Right (ZeroPageX w8) ->
      void $ traverse assembleROM [0x35, w8]
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0x2d, ll, hh]
    Right (AbsoluteX ll hh) ->
      void $ traverse assembleROM [0x3d, ll, hh]
    Right (AbsoluteY ll hh) ->
      void $ traverse assembleROM [0x39, ll, hh]
    Right (IndirectX w8) ->
      void $ traverse assembleROM [0x21, w8]
    Right (IndirectY w8) ->
      void $ traverse assembleROM [0x31, w8]
    Right (Label text) -> K.do
      res <- resolveLabel text
      and res
    _ -> K.log K.Warn ("Invalid operand for AND: " <> oprText)

asl :: T.Text -> Korigatachi ()
asl oprText = K.do
  K.codeGen (spacing <> "asl " <> oprText)
  let
    parseASL =
      parseImmediate <|>
      parseZeroPage <|>
      parseZeroPageX <|>
      parseAbsolute <|>
      parseAbsoluteX <|>
      parseLabel
  case Attoparsec.parseOnly parseASL oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right Accumulator ->
      void $ traverse assembleROM [0x0a]
    Right (ZeroPage w8) ->
      void $ traverse assembleROM [0x06, w8]
    Right (ZeroPageX w8) ->
      void $ traverse assembleROM [0x16, w8]
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0x0e, ll, hh]
    Right (AbsoluteX ll hh) ->
      void $ traverse assembleROM [0x1e, ll, hh]
    Right (Label text) -> K.do
      res <- resolveLabel text
      asl res
    _ -> K.log K.Warn ("Invalid operand for ASL: " <> oprText)

bne :: T.Text -> Korigatachi ()
bne oprText = K.do
  let
    parseBNE =
      parseRelative <|>
      parseLabel
  K.codeGen (spacing <> "bne " <> oprText)
  case Attoparsec.parseOnly parseBNE oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Relative w8) ->
      void $ traverse assembleROM [0xD0, w8]
    Right (Label text) -> K.do -- i hate this operand bullshit
      res <- resolveLabel text
      bne res
    _ -> K.log K.Warn ("Invalid operand for BNE: " <> oprText)

cld :: Korigatachi ()
cld = K.do
  K.codeGen (spacing <> "cld")
  assembleROM 0xD8

dex :: Korigatachi ()
dex = K.do
  K.codeGen (spacing <> "dex")
  assembleROM 0xCA

dey :: Korigatachi ()
dey = K.do
  K.codeGen (spacing <> "dey")
  assembleROM 0x88

jmp :: T.Text -> Korigatachi ()
jmp oprText = K.do
  K.codeGen (spacing <> "JMP " <> oprText)
  let
    parseJMP =
      parseAbsolute <|>
      parseIndirect <|>
      parseLabel
  case Attoparsec.parseOnly parseJMP oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0x4C, ll, hh]
    Right (Indirect ll hh) ->
      void $ traverse assembleROM [0x6C, ll, hh]
    Right (Label text) -> K.do
      res <- resolveLabel text
      jmp res
    _ -> K.log K.Warn ("Invalid operand for JMP: " <> oprText)

lda :: T.Text -> Korigatachi ()
lda oprText = K.do
  K.codeGen (spacing <> "lda " <> oprText)
  case Attoparsec.parseOnly parseOperand oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) ->
      void $ traverse assembleROM [0xA9, w8]
    Right (ZeroPage w8) ->
      void $ traverse assembleROM [0xA5, w8]
    Right (ZeroPageX w8) ->
      void $ traverse assembleROM [0xB5, w8]
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0xAD, ll, hh]
    Right (AbsoluteX ll hh) ->
      void $ traverse assembleROM [0xBD, ll, hh]
    Right (AbsoluteY ll hh) ->
      void $ traverse assembleROM [0xB9, ll, hh]
    Right (IndirectX w8) ->
      void $ traverse assembleROM [0xA1, w8]
    Right (IndirectY w8) ->
      void $ traverse assembleROM [0xB1, w8]
    _ -> K.log K.Warn ("Invalid operand for LDA: " <> oprText)

-- lda oprText = instruct LDA oprText $ \opr -> K.do
--   val <- readOpr opr
--   K.modify $ #cpu % #generalRegisters % #a .~ val

ldx :: T.Text -> Korigatachi ()
ldx oprText = K.do
  K.codeGen (spacing <> "ldx " <> oprText)
  case Attoparsec.parseOnly parseOperand oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) ->
      void $ traverse assembleROM [0xA2, w8]
    Right (ZeroPage w8) ->
      void $ traverse assembleROM [0xA6, w8]
    Right (ZeroPageY w8) ->
      void $ traverse assembleROM [0xB6, w8]
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0xAE, ll, hh]
    Right (AbsoluteY ll hh) ->
      void $ traverse assembleROM [0xBE, ll, hh]
    _ -> K.log K.Warn ("Invalid operand for LDX: " <> oprText)

ldy :: T.Text -> Korigatachi ()
ldy oprText = K.do
  K.codeGen (spacing <> "ldy " <> oprText)
  case Attoparsec.parseOnly parseOperand oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (Immediate w8) ->
      void $ traverse assembleROM [0xA0, w8]
    Right (ZeroPage w8) ->
      void $ traverse assembleROM [0xA4, w8]
    Right (ZeroPageX w8) ->
      void $ traverse assembleROM [0xB4, w8]
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0xAC, ll, hh]
    Right (AbsoluteX ll hh) ->
      void $ traverse assembleROM [0xBC, ll, hh]
    _ -> K.log K.Warn ("Invalid operand for LDY: " <> oprText)

sei :: Korigatachi ()
sei = K.do
  K.codeGen (spacing <> "sei")
  assembleROM 0x78

-- sei :: Korigatachi ()
-- sei = instruct SEI "" (\_ -> setFlags 0b00000100)

sta :: T.Text -> Korigatachi ()
sta oprText = K.do
  K.codeGen (spacing <> "sta " <> oprText)
  case Attoparsec.parseOnly parseOperand oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) ->
      void $ traverse assembleROM [0x85, w8]
    Right (ZeroPageX w8) ->
      void $ traverse assembleROM [0x95, w8]
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0x8D, ll, hh]
    Right (AbsoluteX ll hh) ->
      void $ traverse assembleROM [0x9D, ll, hh]
    Right (AbsoluteY ll hh) ->
      void $ traverse assembleROM [0x99, ll, hh]
    Right (IndirectX w8) ->
      void $ traverse assembleROM [0x81, w8]
    Right (IndirectY w8) ->
      void $ traverse assembleROM [0x91, w8]
    opr -> K.log K.Warn ("Invalid operand for STA: " <> oprText <> " " <> (T.show opr))

-- sta :: T.Text -> Korigatachi ()
-- sta oprText = instruct STA oprText $ \opr ->
--   K.do
--     atari <- K.get
--     write atari.cpu.generalRegisters.a opr

stx :: T.Text -> Korigatachi ()
stx oprText = K.do
  K.codeGen (spacing <> "stx " <> oprText)
  case Attoparsec.parseOnly parseOperand oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) ->
      void $ traverse assembleROM [0x86, w8]
    Right (ZeroPageY w8) ->
      void $ traverse assembleROM [0x96, w8]
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0x8E, ll, hh]
    _ -> K.log K.Warn ("Invalid operand for stX: " <> oprText)

sty :: T.Text -> Korigatachi ()
sty oprText = K.do
  K.codeGen (spacing <> "sty " <> oprText)
  case Attoparsec.parseOnly parseOperand oprText of
    Left _ -> K.log K.Warn ("Failed to parse operand: " <> oprText)
    Right (ZeroPage w8) ->
      void $ traverse assembleROM [0x84, w8]
    Right (ZeroPageX w8) ->
      void $ traverse assembleROM [0x94, w8]
    Right (Absolute ll hh) ->
      void $ traverse assembleROM [0x8C, ll, hh]
    _ -> K.log K.Warn ("Invalid operand for stY: " <> oprText)

tax :: Korigatachi ()
tax = K.do
  K.codeGen (spacing <> "tax")
  assembleROM 0xaa

tay :: Korigatachi ()
tay = K.do
  K.codeGen (spacing <> "tay")
  assembleROM 0xa8

tsx :: Korigatachi ()
tsx = K.do
  K.codeGen (spacing <> "tsx")
  assembleROM 0xBA

txa :: Korigatachi ()
txa = K.do
  K.codeGen (spacing <> "txa")
  assembleROM 0x8a

txs :: Korigatachi ()
txs = K.do
  K.codeGen (spacing <> "txs")
  assembleROM 0x9a

tya :: Korigatachi ()
tya = K.do
  K.codeGen (spacing <> "tya")
  assembleROM 0x98

-- txs :: Korigatachi ()
-- txs = instruct TXS "" $ \_ ->
--   K.modify $
--     \atari -> atari & #cpu % #stackPointer .~ (atari ^. #cpu % #generalRegisters % #x)

advanceTV :: K.Instruction -> Korigatachi ()
advanceTV ins =
  K.modify (\atari -> atari {K.tv = K.advanceTV ins.cycles atari.tv})

-- Weird trick: change the transition state and then just put it back later.
-- K.do
--   atari <- K.get
--   K.modify (const ())
--   K.modify (const atari)
