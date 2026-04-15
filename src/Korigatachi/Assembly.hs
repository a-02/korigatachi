{-# LANGUAGE BinaryLiterals #-}
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

import Data.Bits
import Data.Char (ord)
import Korigatachi.Control qualified as K
import Korigatachi.Model (Korigatachi, Shorthand (..))
import Korigatachi.Model qualified as K
import Korigatachi.Monad qualified as K

sample :: Korigatachi ()
sample = K.do
  sta WSYNC

-- | The standard Atari 2600 start script.
start :: Korigatachi ()
start = K.do
  sei
  cld
  ldx "#$FF"
  

-- sei -- Set Interrupt Disable Flag.
-- cld
-- ldx "#$FF"
-- txs
-- lda "#$00"

ldx :: Operand -> Korigatachi ()
ldx opr = case lookupInstruction LDX opr of
  Nothing ->
    K.katteyomi "LDX called with incompatible operand" ""
  Just instruction -> K.do
    env <- K.ask
    K.when env.assembler $ K.do
      K.katteyomi "" (T.pack $ "ldx " <> opr ^. oprIso)
      writeROMInternal instruction.opcode
      writeROM opr
    K.when env.emulator $ K.do
      atari <- K.get
      writeRAM atari.cpu.generalRegisters.a opr
    K.when env.display $ K.do
      K.modify (\atari -> atari {K.tv = K.advanceTV instruction.cycles atari.tv})


cld :: Korigatachi ()
cld = case lookupInstruction SEI Implied of
  Nothing ->
    K.katteyomi "Couldn't find SEI in lookup table?" ""
  Just instruction -> K.do
    env <- K.ask
    K.when env.assembler $ K.do
      K.katteyomi "" "cld"
      writeROMInternal instruction.opcode
    K.when env.emulator $ K.do
      clearFlags 8
    K.when env.display $ K.do
      K.modify (\atari -> atari {K.tv = K.advanceTV instruction.cycles atari.tv})

sei :: Korigatachi ()
sei = case lookupInstruction SEI Implied of
  Nothing ->
    K.katteyomi "Couldn't find SEI in lookup table?" ""
  Just instruction -> K.do
    env <- K.ask
    K.when env.assembler $ K.do
      K.katteyomi "" "sei"
      writeROMInternal instruction.opcode
    K.when env.emulator $ K.do
      setFlags 4
      -- I'm doing this just because it's funny.
      -- This is the same as setFlags 0b00000100
    K.when env.display $ K.do
      K.modify (\atari -> atari {K.tv = K.advanceTV instruction.cycles atari.tv})

sta :: Operand -> Korigatachi ()
sta opr = case lookupInstruction STA opr of
  Nothing ->
    K.katteyomi "STA called with incompatible operand" ""
  Just instruction -> K.do
    env <- K.ask
    K.when env.assembler $ K.do
      K.katteyomi "" (T.pack $ "sta " <> opr ^. oprIso)
      writeROMInternal instruction.opcode
      writeROM opr
    K.when env.emulator $ K.do
      atari <- K.get
      writeRAM atari.cpu.generalRegisters.a opr
    K.when env.display $ K.do
      K.modify (\atari -> atari {K.tv = K.advanceTV instruction.cycles atari.tv})

-- Weird trick: change the transition state and then just put it back later.
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
      InsOrd.elems $
        InsOrd.filter
          ( \instruction ->
              (instruction.shorthand == sh)
                && (instruction.addressingMode == toAddressingMode opr)
          )
          K.validInstructions
  in
    hashMap !? 0

-- | Write to RAM.
writeRAMInternal :: Word8 -> Word8 -> Korigatachi ()
writeRAMInternal w8 memoryAddress = K.do
  atari <- K.get
  case K.updateMemory atari.ram w8 memoryAddress of
    Left err -> K.katteyomi err ""
    Right mem ->
      K.put (atari & #ram .~ mem)

-- | Write to ROM.
writeROMInternal :: Word8 -> Korigatachi ()
writeROMInternal w8 = K.do
  atari <- K.get
  case K.updateRom atari.rom w8 of
    Left err -> K.katteyomi err ""
    Right rom4k ->
      K.put (atari & #rom .~ rom4k)

-- | TODO: Lensify this better.
setFlags :: Word8 -> Korigatachi ()
setFlags flagsW8 = K.do
  atari <- K.get
  let
    newFlags = flagsIso # (flagsW8 .|. (atari.cpu.statusRegister ^. flagsIso))
  K.put atari {K.cpu = atari ^. #cpu & #statusRegister .~ newFlags}

-- | TODO: Lensify this better.
-- ???
-- clearFlags flags = K.modify $ #cpu . #statusRegister . flagsIso %~ (.&. complement flags)
clearFlags :: Word8 -> Korigatachi ()
clearFlags flagsW8 = K.do
  atari <- K.get
  let
    newFlags = flagsIso # (complement flagsW8 .&. (atari.cpu.statusRegister ^. flagsIso))
  K.put atari {K.cpu = atari ^. #cpu & #statusRegister .~ newFlags}

-- Do the addressing mode calculations here, pass off to necessary writing functions.
write :: Word8 -> Operand -> Korigatachi ()
write val opr = case opr of
  undefined

-- | TODO: Write out all the varying addressing mode behavior for writeOperandRAM and writeOperandROM
writeRAM :: Word8 -> Operand -> Korigatachi ()
writeRAM val opr = case opr of
  Accumulator -> K.ixpure ()
  Implied -> K.ixpure ()
  Immediate w8 -> writeRAMInternal val w8
  IndirectX w8 -> writeRAMInternal val w8
  IndirectY w8 -> writeRAMInternal val w8
  Relative w8 -> writeRAMInternal val w8
  ZeroPage w8 -> writeRAMInternal val w8
  ZeroPageX w8 -> K.do
    atari <- K.get
    writeRAMInternal val (w8 + atari.cpu.generalRegisters.x)
  ZeroPageY w8 -> K.do
    atari <- K.get
    writeRAMInternal val (w8 + atari.cpu.generalRegisters.y)
  Absolute _ _ ->
    K.katteyomi "The Atari can only access 128 bytes of RAM. What are you doing trying to access a 16-bit memory address?" ""
  AbsoluteX _ _ ->
    K.katteyomi "The Atari can only access 128 bytes of RAM. What are you doing trying to access a 16-bit memory address?" ""
  AbsoluteY _ _ ->
    K.katteyomi "The Atari can only access 128 bytes of RAM. What are you doing trying to access a 16-bit memory address?" ""
  Indirect _ _ ->
    K.katteyomi "The Atari can only access 128 bytes of RAM. What are you doing trying to access a 16-bit memory address?" ""
  Unrecognized err -> K.katteyomi (T.pack err) ""

-- | Writing to ROM doesn't follow addressing mode specificities since its not an action
-- undertaken by the Atari itself, rather us creating a binary. All we care are how many bytes
-- written to ROM.
writeROM :: Operand -> Korigatachi ()
writeROM = \case
  Accumulator -> K.ixpure ()
  Implied -> K.ixpure ()
  Immediate w8 -> writeROMInternal w8
  IndirectX w8 -> writeROMInternal w8
  IndirectY w8 -> writeROMInternal w8
  Relative w8 -> writeROMInternal w8
  ZeroPage w8 -> writeROMInternal w8
  ZeroPageX w8 -> writeROMInternal w8
  ZeroPageY w8 -> writeROMInternal w8
  Absolute ll hh ->
    K.do
      writeROMInternal ll
      writeROMInternal hh
  AbsoluteX ll hh ->
    K.do
      writeROMInternal ll
      writeROMInternal hh
  AbsoluteY ll hh ->
    K.do
      writeROMInternal ll
      writeROMInternal hh
  Indirect _ll _hh ->
    -- Did you know? The only instruction the 6507 (nee 6502) that uses
    -- indirect addressing is JMP ($6C).
    -- I just thought that was interesting.
    -- Perhaps this should throw an error instead, but lookupInstruction
    -- already handles the bulk of that work.
    K.ixpure ()
  Unrecognized err -> K.katteyomi (T.pack err) ""

-- Once I figure out how to write sequence_ for indexed monads, I'll write this.
-- burns :: [Word8] -> Korigatachi ()
-- burns w8s = sequence_ $ burn <$> w8s

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
  Unrecognized _ -> K.Implied

oprIso :: Iso' Operand String
oprIso = iso operandToString stringToOperand

flagsIso :: Iso' K.Flags Word8
flagsIso = iso K.flagsToWord8 K.word8ToFlags

instance IsString Operand where
  fromString = (oprIso #) -- I did this just to confuse myself.

operandToString :: Operand -> String
operandToString Accumulator = "A" -- For completeness.
operandToString Implied = "Implied" -- For completeness.
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
operandToString (Unrecognized unrecognized) = unrecognized

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
      pure $ (shiftNibble 0 lowerHalf) .|. (shiftNibble 1 upperHalf)

    parseAccumulator = "A" *> (pure Accumulator) -- You almost never need to do this.

    parseImplied = "Implied" *> (pure Implied)

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
      ll <- parseWord8
      hh <- parseWord8
      pure $ Absolute ll hh

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
    fromMaybe (Unrecognized operand) $ maybeResult (parse parseOperand opr)

pattern VSYNC :: Operand
pattern VSYNC = ZeroPage 0x00 -- Vertical Sync Set-Clear
pattern VBLANK :: Operand
pattern VBLANK = ZeroPage 0x01 -- Vertical Blank Set-Clear
pattern WSYNC :: Operand
pattern WSYNC = ZeroPage 0x02 -- Wait for Horizontal Blank
pattern RSYNC :: Operand
pattern RSYNC = ZeroPage 0x03 -- Reset Horizontal Sync Counter
pattern NUSIZ0 :: Operand
pattern NUSIZ0 = ZeroPage 0x04 -- Number-Size player/missle 0
pattern NUSIZ1 :: Operand
pattern NUSIZ1 = ZeroPage 0x05 -- Number-Size player/missle 1
pattern COLUP0 :: Operand
pattern COLUP0 = ZeroPage 0x06 -- Color-Luminance Player 0
pattern COLUP1 :: Operand
pattern COLUP1 = ZeroPage 0x07 -- Color-Luminance Player 1
pattern COLUPF :: Operand
pattern COLUPF = ZeroPage 0x08 -- Color-Luminance Playfield
pattern COLUBK :: Operand
pattern COLUBK = ZeroPage 0x09 -- Color-Luminance Background
pattern CTRLPF :: Operand
pattern CTRLPF = ZeroPage 0x0A -- Control Playfield, Ball, Collisions
pattern REFP0 :: Operand
pattern REFP0 = ZeroPage 0x0B -- Reflection Player 0
pattern REFP1 :: Operand
pattern REFP1 = ZeroPage 0x0C -- Reflection Player 1
pattern PF0 :: Operand
pattern PF0 = ZeroPage 0x0D -- Playfield Register Byte 0
pattern PF1 :: Operand
pattern PF1 = ZeroPage 0x0E -- Playfield Register Byte 1
pattern PF2 :: Operand
pattern PF2 = ZeroPage 0x0F -- Playfield Register Byte 2
pattern RESP0 :: Operand
pattern RESP0 = ZeroPage 0x10 -- Reset Player 0
pattern RESP1 :: Operand
pattern RESP1 = ZeroPage 0x11 -- Reset Player 1
pattern RESM0 :: Operand
pattern RESM0 = ZeroPage 0x12 -- Reset Missle 0
pattern RESM1 :: Operand
pattern RESM1 = ZeroPage 0x13 -- Reset Missle 1
pattern RESBL :: Operand
pattern RESBL = ZeroPage 0x14 -- Reset Ball
pattern AUDC0 :: Operand
pattern AUDC0 = ZeroPage 0x15 -- Audio Control 0
pattern AUDC1 :: Operand
pattern AUDC1 = ZeroPage 0x16 -- Audio Control 1
pattern AUDF0 :: Operand
pattern AUDF0 = ZeroPage 0x17 -- Audio Frequency 0
pattern AUDF1 :: Operand
pattern AUDF1 = ZeroPage 0x18 -- Audio Frequency 1
pattern AUDV0 :: Operand
pattern AUDV0 = ZeroPage 0x19 -- Audio Volume 0
pattern AUDV1 :: Operand
pattern AUDV1 = ZeroPage 0x1A -- Audio Volume 1
pattern GRP0 :: Operand
pattern GRP0 = ZeroPage 0x1B -- Graphics Register Player 0
pattern GRP1 :: Operand
pattern GRP1 = ZeroPage 0x1C -- Graphics Register Player 1
pattern ENAM0 :: Operand
pattern ENAM0 = ZeroPage 0x1D -- Graphics Enable Missle 0
pattern ENAM1 :: Operand
pattern ENAM1 = ZeroPage 0x1E -- Graphics Enable Missle 1
pattern ENABL :: Operand
pattern ENABL = ZeroPage 0x1F -- Graphics Enable Ball
pattern HMP0 :: Operand
pattern HMP0 = ZeroPage 0x20 -- Horizontal Motion Player 0
pattern HMP1 :: Operand
pattern HMP1 = ZeroPage 0x21 -- Horizontal Motion Player 1
pattern HMM0 :: Operand
pattern HMM0 = ZeroPage 0x22 -- Horizontal Motion Missle 0
pattern HMM1 :: Operand
pattern HMM1 = ZeroPage 0x23 -- Horizontal Motion Missle 1
pattern HMBL :: Operand
pattern HMBL = ZeroPage 0x24 -- Horizontal Motion Ball
pattern VDELP0 :: Operand
pattern VDELP0 = ZeroPage 0x25 -- Vertical Delay Player 0
pattern VDELP1 :: Operand
pattern VDELP1 = ZeroPage 0x26 -- Vertical Delay Player 1
pattern VDELBL :: Operand
pattern VDELBL = ZeroPage 0x27 -- Vertical Delay Ball
pattern RESMP0 :: Operand
pattern RESMP0 = ZeroPage 0x28 -- Reset Missle 0 to Player 0
pattern RESMP1 :: Operand
pattern RESMP1 = ZeroPage 0x29 -- Reset Missle 1 to Player 1
pattern HMOVE :: Operand
pattern HMOVE = ZeroPage 0x2A -- Apply Horizontal Motion
pattern HMCLR :: Operand
pattern HMCLR = ZeroPage 0x2B -- Clear Horizontal Move Registers
pattern CXCLR :: Operand
pattern CXCLR = ZeroPage 0x2C -- Clear Collision Latches
