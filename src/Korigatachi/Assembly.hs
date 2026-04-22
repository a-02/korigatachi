{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}

{- HLINT ignore "Use $>" -}

module Korigatachi.Assembly where

-- base
import Data.Bits
import Data.List
import Data.Word (Word16, Word8)

-- insert-ordered-containers
import Data.HashMap.Strict.InsOrd qualified as InsOrd

-- optics
import Optics

-- text
import Data.Text qualified as T

-- korigatachi
 
import Korigatachi.Assembly.Operand
import Korigatachi.Assembly.Pattern
import Korigatachi.Control qualified as K
import Korigatachi.Model (Korigatachi, Shorthand (..))
import Korigatachi.Model qualified as K
import Korigatachi.Monad qualified as K
import Data.Vector.Sized (index)
import Data.Finite (finite)
import Prelude hiding (read)

sample :: Korigatachi ()
sample = K.do
  sta WSYNC

-- | The standard Atari 2600 start script.
start :: Korigatachi ()
start = K.do
  sei
  cld
  ldx "#$FF"
  txs
  lda "#$00"

instruct :: Shorthand -> Operand -> Korigatachi () -> Korigatachi ()
instruct sh opr emulate = 
  case lookupInstruction sh opr of
  Nothing ->
    K.katteyomi ("Coudn't find instruction in lookup table: " <> T.pack (show sh) <> " " <> T.pack (show opr)) ""
  Just instruction -> K.do
    env <- K.ask
    K.when env.assembler $ K.do
      K.katteyomi "" (T.toLower . T.pack $ show sh <> " " <> opr ^. oprIso)
      assembleROMInternal instruction.opcode
    K.when env.emulator $ emulate
    K.when env.display $ K.do
      advanceTV instruction

cld :: Korigatachi ()
cld = instruct CLD Implied (clearFlags 0b000010000)

lda :: Operand -> Korigatachi ()
lda opr = instruct LDA opr $ K.do
  val <- read opr
  K.modify $ #cpu % #generalRegisters % #a .~ val
  
ldx :: Operand -> Korigatachi ()
ldx opr = instruct LDX opr $ K.do
  val <- read opr
  K.modify $ #cpu % #generalRegisters % #x .~ val

ldy :: Operand -> Korigatachi ()
ldy opr = instruct LDY opr $ K.do
  val <- read opr
  K.modify $ #cpu % #generalRegisters % #y .~ val

sei :: Korigatachi ()
sei = instruct SEI Implied (setFlags 0b00000100)

sta :: Operand -> Korigatachi ()
sta opr = instruct STA opr $
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.a opr

txs :: Korigatachi ()
txs = case lookupInstruction TXS Implied of
  Nothing ->
    K.katteyomi "LDX called with incompatible operand" ""
  Just instruction -> K.do
    env <- K.ask
    K.when env.assembler $ K.do
      K.katteyomi "" "txs"
      assembleROMInternal instruction.opcode
    K.when env.emulator $ K.do
      clearFlags 0
    K.when env.display $ K.do
      advanceTV instruction

advanceTV :: K.Instruction -> Korigatachi ()
advanceTV ins =
  K.modify (\atari -> atari {K.tv = K.advanceTV ins.cycles atari.tv})

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

-- | Assemble ROM incrementally.
assembleROMInternal :: Word8 -> Korigatachi ()
assembleROMInternal w8 = K.do
  atari <- K.get
  case K.updateRom atari.rom w8 of
    Left err -> K.katteyomi err ""
    Right rom4k ->
      K.put (atari & #rom .~ rom4k)

setFlags :: Word8 -> Korigatachi ()
setFlags flagsW8 = K.modify $ #cpu % #statusRegister %~ (flagsIso %~ (flagsW8 .|.))

clearFlags :: Word8 -> Korigatachi ()
clearFlags flagsW8 = K.modify $ #cpu % #statusRegister %~ (flagsIso %~ (complement flagsW8 .&.))

write :: Word8 -> Operand -> Korigatachi ()
write val opr = K.do
  oprW16 <- operandToWord16 opr
  let
    go
      | oprW16 >= 0x00 && oprW16 <= 0x2C = writeTIA val oprW16 -- TIA registers
      | oprW16 >= 0x80 && oprW16 <= 0xFF = writeRAMInternal val (fromIntegral oprW16)
      | oprW16 >= 0x280 && oprW16 <= 0x297 = writePIA val oprW16 -- PIA registers
      | oprW16 >= 0xF000 && oprW16 <= 0xFFFF -- ROM
        =
          K.katteyomi "Attempted to write to ROM?!" ""
      | otherwise = K.ixpure ()
  go

read :: Operand -> Korigatachi Word8
read opr = K.do
  oprW16 <- operandToWord16 opr
  let
    go
      | oprW16 >= 0x00 && oprW16 <= 0x0D = readTIA oprW16 -- TIA registers
      | oprW16 >= 0x80 && oprW16 <= 0xFF = readRAM oprW16 -- RAM
      | oprW16 >= 0x280 && oprW16 <= 0x297 = readPIA oprW16 -- PIA registers
      | oprW16 >= 0x1000 && oprW16 <= 0x1FFF = readROM oprW16 -- ROM reflections
      | oprW16 >= 0x3000 && oprW16 <= 0x3FFF = readROM oprW16
      | oprW16 >= 0x5000 && oprW16 <= 0x5FFF = readROM oprW16
      | oprW16 >= 0x7000 && oprW16 <= 0x7FFF = readROM oprW16
      | oprW16 >= 0x9000 && oprW16 <= 0x9FFF = readROM oprW16
      | oprW16 >= 0xB000 && oprW16 <= 0xBFFF = readROM oprW16
      | oprW16 >= 0xD000 && oprW16 <= 0xDFFF = readROM oprW16
      | oprW16 >= 0xF000 && oprW16 <= 0xFFFF = readROM oprW16
      | otherwise = K.do
          K.katteyomi ("Attempted to read invalid address: " <> T.pack (oprW16 ^. K.hex16)) ""
          K.ixpure 0
  go

readROM :: Word16 -> Korigatachi Word8
readROM w16 = K.do
  let
    fin = finite (fromIntegral $ w16 .&. 0x0FFF) -- Can only see the 12 least significant bits.
  K.query $ \a -> a.rom.memory4k `index` fin

readPIA :: Word16 -> Korigatachi Word8
readPIA oprW16 = 
  case oprW16 of
    0x280 -> K.query $ \a -> a.pia.swcha
    0x281 -> K.query $ \a -> a.pia.swacnt
    0x282 -> K.query $ \a -> a.pia.swchb
    0x283 -> K.query $ \a -> a.pia.swbcnt
    0x284 -> K.query $ \a -> a.pia.intim
    0x294 -> K.query $ \a -> a.pia.tim1t
    0x295 -> K.query $ \a -> a.pia.tim8t
    0x296 -> K.query $ \a -> a.pia.tim64t
    0x297 -> K.query $ \a -> a.pia.t1024t
    _ -> K.do
       K.katteyomi ("Attempted to read invalid PIA register: " <> T.pack (oprW16 ^. K.hex16)) ""
       K.ixpure 0

readRAM :: Word16 -> Korigatachi Word8
readRAM w16 = K.do
  let
    fin = finite (fromIntegral w16 - 0x80) -- Putting it back in bounds of the sized vector.
  K.query $ \a -> a.ram `index` fin

readTIA :: Word16 -> Korigatachi Word8
readTIA oprW16 = K.do
  case oprW16 of
    0x00 -> K.query $ \a -> a.tia.read.cxm0p 
    0x01 -> K.query $ \a -> a.tia.read.cxm1p 
    0x02 -> K.query $ \a -> a.tia.read.cxp0fb 
    0x03 -> K.query $ \a -> a.tia.read.cxp1fb 
    0x04 -> K.query $ \a -> a.tia.read.cxm0fb 
    0x05 -> K.query $ \a -> a.tia.read.cxm1fb 
    0x06 -> K.query $ \a -> a.tia.read.cxblpf 
    0x07 -> K.query $ \a -> a.tia.read.cxppmm 
    0x08 -> K.query $ \a -> a.tia.read.inpt0 
    0x09 -> K.query $ \a -> a.tia.read.inpt1 
    0x0A -> K.query $ \a -> a.tia.read.inpt2 
    0x0B -> K.query $ \a -> a.tia.read.inpt3 
    0x0C -> K.query $ \a -> a.tia.read.inpt4 
    0x0D -> K.query $ \a -> a.tia.read.inpt5 
    _ -> K.do
      K.katteyomi ("Attempted to read invalid TIA register: " <> T.pack (oprW16 ^. K.hex16)) ""
      K.ixpure 0

-- It should be noted that while these TIA registers can be written to, it's not possible
-- to read from them again. The data is essentially hoarded by the TIA. We keep the data around
-- for debugging purposes. Also cause its hard to model it any other way.
writeTIA :: Word8 -> Word16 -> Korigatachi ()
writeTIA val oprW16 = K.do
  case oprW16 of
    0x00 -> K.modify $ #tia % #write % #vsync .~ val
    0x01 -> K.modify $ #tia % #write % #vblank .~ val
    0x02 -> K.modify $ #tia % #write % #wsync .~ val -- TODO: There's some special behavior that happens when this is strobed.
    0x03 -> K.modify $ #tia % #write % #rsync .~ val
    0x04 -> K.modify $ #tia % #write % #nusiz0 .~ val
    0x05 -> K.modify $ #tia % #write % #nusiz1 .~ val
    0x06 -> K.modify $ #tia % #write % #colup0 .~ val
    0x07 -> K.modify $ #tia % #write % #colup1 .~ val
    0x08 -> K.modify $ #tia % #write % #colupf .~ val
    0x09 -> K.modify $ #tia % #write % #colubk .~ val
    0x0A -> K.modify $ #tia % #write % #ctrlpf .~ val
    0x0B -> K.modify $ #tia % #write % #refp0 .~ val
    0x0C -> K.modify $ #tia % #write % #refp1 .~ val
    0x0D -> K.modify $ #tia % #write % #pf0 .~ val
    0x0E -> K.modify $ #tia % #write % #pf1 .~ val
    0x0F -> K.modify $ #tia % #write % #pf2 .~ val
    0x10 -> K.modify $ #tia % #write % #resp0 .~ val
    0x11 -> K.modify $ #tia % #write % #resp1 .~ val
    0x12 -> K.modify $ #tia % #write % #resm0 .~ val
    0x13 -> K.modify $ #tia % #write % #resm1 .~ val
    0x14 -> K.modify $ #tia % #write % #resbl .~ val
    0x15 -> K.modify $ #tia % #write % #audc0 .~ val
    0x16 -> K.modify $ #tia % #write % #audc1 .~ val
    0x17 -> K.modify $ #tia % #write % #audf0 .~ val
    0x18 -> K.modify $ #tia % #write % #audf1 .~ val
    0x19 -> K.modify $ #tia % #write % #audv0 .~ val
    0x1A -> K.modify $ #tia % #write % #audv1 .~ val
    0x1B -> K.modify $ #tia % #write % #grp0 .~ val
    0x1C -> K.modify $ #tia % #write % #grp1 .~ val
    0x1D -> K.modify $ #tia % #write % #enam0 .~ val
    0x1E -> K.modify $ #tia % #write % #enam1 .~ val
    0x1F -> K.modify $ #tia % #write % #enabl .~ val
    0x20 -> K.modify $ #tia % #write % #hmp0 .~ val
    0x21 -> K.modify $ #tia % #write % #hmp1 .~ val
    0x22 -> K.modify $ #tia % #write % #hmm0 .~ val
    0x23 -> K.modify $ #tia % #write % #hmm1 .~ val
    0x24 -> K.modify $ #tia % #write % #hmbl .~ val
    0x25 -> K.modify $ #tia % #write % #vdelp0 .~ val
    0x26 -> K.modify $ #tia % #write % #vdelp1 .~ val
    0x27 -> K.modify $ #tia % #write % #vdelbl .~ val
    0x28 -> K.modify $ #tia % #write % #resmp0 .~ val
    0x29 -> K.modify $ #tia % #write % #resmp1 .~ val
    0x2A -> K.modify $ #tia % #write % #hmove .~ val
    0x2B -> K.modify $ #tia % #write % #hmclr .~ val
    0x2C -> K.modify $ #tia % #write % #cxclr .~ val -- TODO: Strobing this clears the collision latches.
    _ -> K.katteyomi ("Attempted to write to invalid TIA register: " <> T.pack (oprW16 ^. K.hex16)) ""

writePIA :: Word8 -> Word16 -> Korigatachi ()
writePIA val oprW16 =
  case oprW16 of
    0x280 -> K.modify $ #pia % #swcha .~ val
    0x281 -> K.modify $ #pia % #swacnt .~ val
    0x282 -> K.katteyomi "SWCHB ($282) is read-only." ""
    0x283 -> K.katteyomi "SWBCNT ($283) is read-only." ""
    0x284 -> K.katteyomi "INTIM ($284) is read-only." ""
    0x294 -> K.modify $ #pia % #tim1t .~ val
    0x295 -> K.modify $ #pia % #tim8t .~ val
    0x296 -> K.modify $ #pia % #tim64t .~ val
    0x297 -> K.modify $ #pia % #t1024t .~ val
    _ -> K.katteyomi ("Attempted to write to invalid PIA register: " <> T.pack (oprW16 ^. K.hex16)) ""

{- | Writing to ROM doesn't follow addressing mode specificities since its not an action
undertaken by the Atari itself, rather us creating a binary. All we care are how many bytes
written to ROM.
-}
assembleROM :: Operand -> Korigatachi ()
assembleROM = \case
  Accumulator -> K.ixpure ()
  Implied -> K.ixpure ()
  Immediate w8 -> assembleROMInternal w8
  IndirectX w8 -> assembleROMInternal w8
  IndirectY w8 -> assembleROMInternal w8
  Relative w8 -> assembleROMInternal w8
  ZeroPage w8 -> assembleROMInternal w8
  ZeroPageX w8 -> assembleROMInternal w8
  ZeroPageY w8 -> assembleROMInternal w8
  Absolute ll hh ->
    K.do
      assembleROMInternal ll
      assembleROMInternal hh
  AbsoluteX ll hh ->
    K.do
      assembleROMInternal ll
      assembleROMInternal hh
  AbsoluteY ll hh ->
    K.do
      assembleROMInternal ll
      assembleROMInternal hh
  Indirect ll hh ->
    -- Did you know? The only instruction the 6507 (nee 6502) that uses
    -- indirect addressing is JMP ($6C).
    -- I just thought that was interesting.
    K.do
      assembleROMInternal ll
      assembleROMInternal hh
  Unrecognized err -> K.katteyomi (T.pack err) ""

-- Once I figure out how to write sequence_ for indexed monads, I'll write this.
-- burns :: [Word8] -> Korigatachi ()
-- burns w8s = sequence_ $ burn <$> w8s
