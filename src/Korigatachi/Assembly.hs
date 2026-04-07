{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}

module Korigatachi.Assembly where

-- attoparsec
import Data.Attoparsec.ByteString.Char8 as Attoparsec

-- base
import Control.Applicative
import Control.Monad (void)
import Data.List
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Word (Word16, Word8)

-- bytestring
import Data.ByteString.Char8 qualified as BSC8

-- insert-ordered-containers
import Data.HashMap.Strict.InsOrd qualified as InsOrd

-- optics
import Optics

-- text
import Data.Text qualified as T

-- korigatachi
import Korigatachi.Control qualified as K
import Korigatachi.Model (Korigatachi, Shorthand(..))
import Korigatachi.Model qualified as K
import Korigatachi.Monad qualified as K

sample :: Korigatachi ()
sample = K.do
  sta WSYNC

-- | The standard Atari 2600 start script.
start :: Korigatachi ()
start = K.do
  undefined
  -- sei -- Set Interrupt Disable Flag.
  -- cld
  -- ldx "#$FF"
  -- txs
  -- lda "#$00"

sta :: Operand -> Korigatachi ()
sta opr = case lookupInstruction STA opr of
  Nothing ->
    K.katteyomi "Incompatible operand" ""
  Just instruction -> K.do
    env <- K.ask
    K.when env.assembler $ K.do
      K.katteyomi "" (T.pack $ "sta " <> opr ^. oprIso)
      writeROM instruction.opcode
      writeOperandROM opr
    K.when env.emulator $ K.do
      atari <- K.get
      writeRAM atari.cpu.generalRegisters.a addr
    K.when env.display $ K.do
      K.modify (\atari -> atari { K.tv = K.advanceTV instruction.cycles atari.tv })
      
-- Weird trick: Rchange the transition state and then just put it back later.
-- K.do
--   atari <- K.get
--   K.modify (const ())
--   K.modify (const atari)

      

-- TODO: Unify "Operand" and "AddressingMode".
-- Something where we don't have to do this lookup every time.
lookupInstruction :: K.Shorthand -> Operand -> Maybe K.Instruction
lookupInstruction sh opr =
  let
    hashMap =
      InsOrd.elems $ InsOrd.filter
        (\instruction ->
            (instruction.shorthand == sh) &&
            (instruction.addressingMode == toAddressingMode opr)
        )        
        K.validInstructions
  in
    hashMap !? 0

-- | Write to RAM.
writeRAM :: Word8 -> Word8 -> Korigatachi ()
writeRAM w8 memoryAddress = K.do
  atari <- K.get
  case K.updateMemory atari.ram w8 memoryAddress of
    Left err -> K.katteyomi err ""
    Right mem ->
      K.put (atari & #ram .~ mem)

-- | Write to ROM.
writeROM :: Word8 -> Korigatachi ()
writeROM w8 = K.do
  atari <- K.get
  case K.updateRom atari.rom w8 of
    Left err -> K.katteyomi err ""
    Right rom4k ->
      K.put (atari & #rom .~ rom4k)

writeOperandROM :: Operand -> Korigatachi ()
writeOperandROM = \case
  Accumulator -> K.ixpure ()
  Implied -> K.ixpure ()
  Immediate w8 -> writeROM w8 
  IndirectX w8 -> writeROM w8 
  IndirectY w8 -> writeROM w8 
  Relative w8 -> writeROM w8 
  ZeroPage w8 -> writeROM w8 
  ZeroPageX w8 -> writeROM w8 
  ZeroPageY w8 -> writeROM w8 
  Absolute ll hh ->
    K.do
      writeROM ll
      writeROM hh 
  AbsoluteX ll hh ->
    K.do
      writeROM ll
      writeROM hh 
  AbsoluteY ll hh ->
    K.do
      writeROM ll
      writeROM hh 
  Indirect ll hh ->
    K.do
      writeROM ll
      writeROM hh 
  Unrecognized err -> K.katteyomi err ""
  

-- Once I figure out how to write sequence_ for indexed monads, I'll write this.
-- burns :: [Word8] -> Korigatachi ()
-- burns w8s = sequence_ $ burn <$> w8s

data Operand
  = Accumulator -- Implied, can't be constructed.
  | Implied -- Same.
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
  | Unrecognized String -- What did you do?

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
  

oprIso :: Iso' Operand String
oprIso = iso operandToString stringToOperand

instance IsString Operand where
  fromString = (oprIso #) -- I did this just to confuse myself.

operandToString :: Operand -> String
operandToString Accumulator = "Accumulator" -- For completeness.
operandToString Implied = "Implied" -- For completeness.
operandToString (Immediate opr) = "#$" <> (opr ^. K.hex8)
operandToString (IndirectX opr) = "($" <> (opr ^. K.hex8) <> ",x)"
operandToString (IndirectY opr) = "($" <> (opr ^. K.hex8) <> "),y"
operandToString (Relative opr) = "r$" <> (opr ^. K.hex8)
operandToString (ZeroPage opr) = "$" <> (opr ^. K.hex8)
operandToString (ZeroPageX opr) = "$" <> (opr ^. K.hex8) <> ",x"
operandToString (ZeroPageY opr) = "$" <> (opr ^. K.hex8) <> ",y"
operandToString (Absolute ll hh) = "$" <> ((hh * 256 + ll) ^. K.hex16) -- order of operations?
operandToString (AbsoluteX ll hh) = "$" <> ((hh * 256 + ll) ^. K.hex16) <> ",x"
operandToString (AbsoluteY ll hh) = "$" <> ((hh * 256 + ll) ^. K.hex16) <> ",y"
operandToString (Indirect ll hh) = "($" <> ((hh * 256 + ll) ^. K.hex16) <> ")"
operandToString (Unrecognized unrecognized) = unrecognized

stringToOperand :: String -> Operand
stringToOperand operand =
  let
    opr = BSC8.pack operand

    parseWord8 = (Attoparsec.take 4) >>= hexadecimal -- each character is 2 bytes, right?

    parseAccumulator = "Accumulator" *> (pure Accumulator)
    
    parseImplied = "Implied" *> (pure Implied)

    parseImmediate = "#$" *> (Immediate <$> hexadecimal)

    parseIndirectX = do
      void $ string "($"
      w8 <- hexadecimal
      void $ string ",x)"
      pure $ IndirectX w8

    parseIndirectY = do
      void $ string "($"
      w8 <- hexadecimal
      void $ string "),y"
      pure $ IndirectX w8

    parseRelative = "r$" *> (Relative <$> hexadecimal)

    parseZeroPage = char '$' *> (ZeroPage <$> hexadecimal)

    parseZeroPageX = do
      void $ char '$'
      w8 <- hexadecimal
      void $ string ",x"
      pure $ ZeroPageX w8

    parseZeroPageY = do
      void $ char '$'
      w8 <- hexadecimal
      void $ string ",y"
      pure $ ZeroPageY w8

    parseAbsolute = char '$' *> (Absolute <$> hexadecimal)

    parseAbsoluteX = do
      void $ char '$'
      w16 <- hexadecimal
      void $ string ",x"
      pure $ AbsoluteX w16

    parseAbsoluteY = do
      void $ char '$'
      w16 <- hexadecimal
      void $ string ",y"
      pure $ AbsoluteY w16

    parseIndirect = do
      void $ string "($"
      w16 <- hexadecimal
      void $ char ')'
      pure $ Indirect w16

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
    fromMaybe (Unrecognized operand) $ maybeResult (parse parseOperand opr)

pattern VSYNC   = ZeroPage 0x00  -- Vertical Sync Set-Clear
pattern VBLANK  = ZeroPage 0x01  -- Vertical Blank Set-Clear
pattern WSYNC   = ZeroPage 0x02  -- Wait for Horizontal Blank
pattern RSYNC   = ZeroPage 0x03  -- Reset Horizontal Sync Counter
pattern NUSIZ0  = ZeroPage 0x04  -- Number-Size player/missle 0
pattern NUSIZ1  = ZeroPage 0x05  -- Number-Size player/missle 1
pattern COLUP0  = ZeroPage 0x06  -- Color-Luminance Player 0
pattern COLUP1  = ZeroPage 0x07  -- Color-Luminance Player 1
pattern COLUPF  = ZeroPage 0x08  -- Color-Luminance Playfield
pattern COLUBK  = ZeroPage 0x09  -- Color-Luminance Background
pattern CTRLPF  = ZeroPage 0x0A  -- Control Playfield, Ball, Collisions
pattern REFP0   = ZeroPage 0x0B  -- Reflection Player 0
pattern REFP1   = ZeroPage 0x0C  -- Reflection Player 1
pattern PF0     = ZeroPage 0x0D  -- Playfield Register Byte 0
pattern PF1     = ZeroPage 0x0E  -- Playfield Register Byte 1
pattern PF2     = ZeroPage 0x0F  -- Playfield Register Byte 2
pattern RESP0   = ZeroPage 0x10  -- Reset Player 0
pattern RESP1   = ZeroPage 0x11  -- Reset Player 1
pattern RESM0   = ZeroPage 0x12  -- Reset Missle 0
pattern RESM1   = ZeroPage 0x13  -- Reset Missle 1
pattern RESBL   = ZeroPage 0x14  -- Reset Ball
pattern AUDC0   = ZeroPage 0x15  -- Audio Control 0
pattern AUDC1   = ZeroPage 0x16  -- Audio Control 1
pattern AUDF0   = ZeroPage 0x17  -- Audio Frequency 0
pattern AUDF1   = ZeroPage 0x18  -- Audio Frequency 1
pattern AUDV0   = ZeroPage 0x19  -- Audio Volume 0
pattern AUDV1   = ZeroPage 0x1A  -- Audio Volume 1
pattern GRP0    = ZeroPage 0x1B  -- Graphics Register Player 0
pattern GRP1    = ZeroPage 0x1C  -- Graphics Register Player 1
pattern ENAM0   = ZeroPage 0x1D  -- Graphics Enable Missle 0
pattern ENAM1   = ZeroPage 0x1E  -- Graphics Enable Missle 1
pattern ENABL   = ZeroPage 0x1F  -- Graphics Enable Ball
pattern HMP0    = ZeroPage 0x20  -- Horizontal Motion Player 0
pattern HMP1    = ZeroPage 0x21  -- Horizontal Motion Player 1
pattern HMM0    = ZeroPage 0x22  -- Horizontal Motion Missle 0
pattern HMM1    = ZeroPage 0x23  -- Horizontal Motion Missle 1
pattern HMBL    = ZeroPage 0x24  -- Horizontal Motion Ball
pattern VDELP0  = ZeroPage 0x25  -- Vertical Delay Player 0
pattern VDELP1  = ZeroPage 0x26  -- Vertical Delay Player 1
pattern VDELBL  = ZeroPage 0x27  -- Vertical Delay Ball
pattern RESMP0  = ZeroPage 0x28  -- Reset Missle 0 to Player 0
pattern RESMP1  = ZeroPage 0x29  -- Reset Missle 1 to Player 1
pattern HMOVE   = ZeroPage 0x2A  -- Apply Horizontal Motion
pattern HMCLR   = ZeroPage 0x2B  -- Clear Horizontal Move Registers
pattern CXCLR   = ZeroPage 0x2C  -- Clear Collision Latches
