{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Korigatachi.Model where

-- finite-typelits
import Data.Finite (finite)

-- insert-ordered-containers
import Data.HashMap.Strict.InsOrd as InsOrd

-- text
import Data.Text qualified as T

-- sized-vector
import Data.Vector.Sized ((//))
import Data.Vector.Sized qualified as Sized

-- base

import Data.Bits (Bits (bit, (.|.)), testBit)
import Data.Word (Word8)
import Prelude hiding (break)

-- korigatachi
import Korigatachi.Monad
import Korigatachi.Types

-- | Write to the writer.
katteyomi :: T.Text -> T.Text -> Korigatachi ()
katteyomi logMsg codeGen = tell (Katteyomi logMsg codeGen)

emptyAtari :: Atari
emptyAtari =
  Atari
    emptyMemory
    emptyCPU
    emptyROM
    emptyTV
    emptyTIA
    emptyPIA

emptyMemory :: Memory
emptyMemory = Sized.replicate 0

updateMemory :: Memory -> Word8 -> Word8 -> Either T.Text Memory
updateMemory memory w8 address =
  if address < 128 -- RAM lives in $80-$FF.
    then
      Left
        "Tried to write to a RAM byte below $80. Perhaps you meant to write to a TIA register?"
    else Right $ memory // [(finite . fromIntegral $ address - 128, w8)] -- Minus 128 to get it in the sized range.

emptyROM :: Memory4K
emptyROM =
  Memory4K
    (Sized.replicate 0xFF)
    0
    []

{- | Hey, son. This might seem a bit odd, but the "focus" here is
what was just written.
-}
updateRom :: Memory4K -> Word8 -> Either T.Text Memory4K
updateRom rom4k w8 =
  if rom4k.focus + 1 >= 4096 -- 0-indexed, remember?
    then Left "ROM Overflow."
    else
      Right $
        Memory4K
          (rom4k.memory4k // [(finite $ fromIntegral (rom4k.focus + 1), w8)])
          (rom4k.focus + 1) -- Hey, this can overflow! Maybe add some error-checking?
          rom4k.labels

emptyTV :: TV
emptyTV =
  TV
    0
    VSync
    0
    0
    NTSC

advanceTV :: Int -> TV -> TV
advanceTV cyclesToAdvance television =
  let
    current = (television.frame * 19912) + (television.line * 76) + television.hPos
    advanced = current + cyclesToAdvance
    (advFrame, remFrame) = advanced `quotRem` 19912 -- 262 * 76
    (advLine, advHPos) = remFrame `quotRem` 76 -- 76
  in
    TV
      { frame = advFrame
      , section = television.section -- TODO: Actually update the section.
      , line = advLine
      , hPos = advHPos
      , region = television.region -- Imagine changing the region of a console while it's on.
      }

emptyPIA :: PIA
emptyPIA = PIA 0 0 0 0 0 0 0 0 0

emptyTIA :: TIA
emptyTIA = TIA emptyWriteTIA emptyReadTIA

emptyReadTIA :: ReadTIA
emptyReadTIA = ReadTIA 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -- lol

emptyWriteTIA :: WriteTIA
emptyWriteTIA = WriteTIA 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -- LOL

emptyCPU :: MOS6507
emptyCPU =
  MOS6507
    emptyRegisters
    0
    0
    (word8ToFlags 0)
    0

emptyRegisters :: Registers
emptyRegisters = Registers 0 0 0

flagsToWord8 :: Flags -> Word8
flagsToWord8 Flags {..} =
  let
    bitBool False _ = 0
    bitBool True x = bit x
  in
    bitBool negative 7
      .|. bitBool overflow 6
      .|. bitBool ignored 5
      .|. bitBool break 4
      .|. bitBool decimal 3
      .|. bitBool interrupt 2
      .|. bitBool zero 1
      .|. bitBool carry 0 -- Strange, isn't it?

word8ToFlags :: Word8 -> Flags
word8ToFlags w8 =
  Flags
    { negative = testBit w8 7
    , overflow = testBit w8 6
    , ignored = testBit w8 5
    , break = testBit w8 4
    , decimal = testBit w8 3
    , interrupt = testBit w8 2
    , zero = testBit w8 1
    , carry = testBit w8 0
    }

type ValidInstructions = InsOrd.InsOrdHashMap Word8 Instruction

validInstructions :: InsOrd.InsOrdHashMap Word8 Instruction
validInstructions =
  InsOrd.fromList $
    zip
      ([0 .. 255] :: [Word8])
      [ Instruction BRK "Implied" 0x00 1 7 "Force Break"
      , Instruction ORA "IndirectX" 0x01 2 6 "OR Memory with Accumulator"
      , Instruction JAM "Implied" 0x02 0 0 "Freeze the CPU"
      , Instruction SLO "IndirectX" 0x03 2 8 "ASL op + ORA op"
      , Instruction NOP "ZeroPage" 0x04 2 3 "No operation"
      , Instruction ORA "ZeroPage" 0x05 2 3 "OR Memory with Accumulator"
      , Instruction ASL "ZeroPage" 0x06 2 5 "Shift Left One Bit"
      , Instruction SLO "ZeroPage" 0x07 2 5 "ASL op + ORA op"
      , Instruction PHP "Implied" 0x08 1 3 "Push Processor Status on Stack"
      , Instruction ORA "Immediate" 0x09 2 2 "OR Memory with Accumulator"
      , Instruction ASL "Accumulator" 0x0A 1 2 "Shift Left One Bit"
      , Instruction ANC "Immediate" 0x0B 2 2 "AND oper + set C as ASL"
      , Instruction NOP "Absolute" 0x0C 3 4 "No operation"
      , Instruction ORA "Absolute" 0x0D 3 4 "OR Memory with Accumulator"
      , Instruction ASL "Absolute" 0x0E 3 6 "Shift Left One Bit"
      , Instruction SLO "Absolute" 0x0F 3 6 "ASL op + ORA op"
      , Instruction BPL "Relative" 0x10 2 2 "Branch on N = 0"
      , Instruction ORA "IndirectY" 0x11 2 5 "OR Memory with Accumulator"
      , Instruction JAM "Implied" 0x12 0 0 "Freeze the CPU"
      , Instruction SLO "IndirectY" 0x13 2 8 "ASL op + ORA op"
      , Instruction NOP "ZeroPageX" 0x14 2 4 "No operation"
      , Instruction ORA "ZeroPageX" 0x15 2 4 "OR Memory with Accumulator"
      , Instruction ASL "ZeroPageX" 0x16 2 6 "Shift Left One Bit"
      , Instruction SLO "ZeroPageX" 0x17 2 5 "ASL op + ORA op"
      , Instruction CLC "Implied" 0x18 1 2 "Clear Carry Flag"
      , Instruction ORA "AbsoluteY" 0x19 3 4 "OR Memory with Accumulator"
      , Instruction NOP "Implied" 0x1A 1 2 "No operation"
      , Instruction SLO "AbsoluteY" 0x1B 3 7 "ASL op + ORA op"
      , Instruction NOP "AbsoluteX" 0x1C 1 2 "No operation"
      , Instruction ORA "AbsoluteX" 0x1D 3 4 "OR Memory with Accumulator"
      , Instruction ASL "AbsoluteX" 0x1E 3 7 "Shift Left One Bit"
      , Instruction SLO "AbsoluteX" 0x1F 3 7 "ASL oper + ORA oper"
      , Instruction JSR "Absolute" 0x20 3 6 "Jump to subroutine"
      , Instruction AND "IndirectX" 0x21 2 6 "AND memory with accumulator"
      , Instruction JAM "Implied" 0x22 0 0 "Freeze the CPU"
      , Instruction RLA "IndirectX" 0x23 2 8 "ROL oper + AND oper"
      , Instruction BIT "ZeroPage" 0x24 2 3 "Test Bits in Memory with accumulator"
      , Instruction AND "ZeroPage" 0x25 2 3 "AND Memory with Accumulator"
      , Instruction ROL "ZeroPage" 0x26 2 5 "Rotate One Bit Left"
      , Instruction RLA "ZeroPage" 0x27 2 5 "ROL oper + AND oper"
      , Instruction PLP "Implied" 0x28 1 4 "Pull processor status from stack"
      , Instruction AND "Immediate" 0x29 2 2 "AND Memory with Accumulator"
      , Instruction ROL "Accumulator" 0x2A 1 2 "Rotate One Bit Left"
      , Instruction ANC "Immediate" 0x2B 2 2 "AND oper + set C as ASL"
      , Instruction BIT "Absolute" 0x2C 3 4 "Test Bits in Memory with accumulator"
      , Instruction AND "Absolute" 0x2D 3 4 "AND memory with accumulator"
      , Instruction ROL "Absolute" 0x2E 3 6 "Rotate One Bit Left"
      , Instruction RLA "Absolute" 0x2F 3 6 "ROL oper + AND oper"
      , Instruction BMI "Relative" 0x30 2 2 "Branch on Result Minus"
      , Instruction AND "IndirectY" 0x31 2 5 "AND Memory with Accumulator"
      , Instruction JAM "Implied" 0x32 0 0 "Freeze the CPU"
      , Instruction RLA "IndirectY" 0x33 2 8 "ROL Oper + AND oper"
      , Instruction NOP "ZeroPageX" 0x34 2 4 "No operation"
      , Instruction AND "ZeroPageX" 0x35 2 4 "AND Memory with Accumulator"
      , Instruction ROL "ZeroPageX" 0x36 2 6 "Rotate One Bit Left"
      , Instruction RLA "ZeroPageX" 0x37 2 6 "ROL Oper + AND oper"
      , Instruction SEC "Implied" 0x38 1 2 "Set Carry Flag"
      , Instruction AND "AbsoluteY" 0x39 3 4 "AND Memory with Accumulator"
      , Instruction NOP "Implied" 0x3A 1 2 "No operation"
      , Instruction RLA "AbsoluteY" 0x3B 3 7 "ROL Oper + AND oper"
      , Instruction NOP "AbsoluteX" 0x3C 3 4 "No operation"
      , Instruction AND "AbsoluteX" 0x3D 3 4 "AND Memory with Accumulator"
      , Instruction ROL "AbsoluteX" 0x3E 3 7 "Rotate One Bit Left"
      , Instruction RLA "AbsoluteX" 0x3F 3 7 "ROL Oper + AND oper"
      , Instruction RTI "Implied" 0x40 1 6 "Return from Interrupt"
      , Instruction EOR "IndirectX" 0x41 2 6 "Exclusive-OR Memory with Accumulator"
      , Instruction JAM "Implied" 0x42 0 0 "Freeze the CPU"
      , Instruction SRE "IndirectX" 0x43 2 8 "LSR oper + EOR oper"
      , Instruction NOP "ZeroPage" 0x44 2 3 "No operation"
      , Instruction EOR "ZeroPage" 0x45 2 3 "Exclusive-OR Memory with Accumulator"
      , Instruction LSR "ZeroPage" 0x46 2 5 "Shift One Bit Right"
      , Instruction SRE "ZeroPage" 0x47 2 5 "LSR oper + EOR oper"
      , Instruction PHA "Implied" 0x48 1 3 "Push Accumulator on Stack"
      , Instruction EOR "Immediate" 0x49 2 2 "Exclusive-OR Memory with Accumulator"
      , Instruction LSR "Accumulator" 0x4A 1 2 "Shift One Bit Right"
      , Instruction ALR "Immediate" 0x4B 2 2 "AND oper + LSR"
      , Instruction JMP "Absolute" 0x4C 3 3 "Jump to New Location"
      , Instruction EOR "Absolute" 0x4D 3 4 "Exclusive-OR Memory with Accumulator"
      , Instruction LSR "Absolute" 0x4E 3 6 "Shift One Bit Right"
      , Instruction SRE "Absolute" 0x4F 3 6 "LSR oper + EOR oper"
      , Instruction BVC "Relative" 0x50 2 2 "Branch on Overflow Clear"
      , Instruction EOR "IndirectY" 0x51 2 5 "Exclusive-OR Memory with Accumulator"
      , Instruction JAM "Implied" 0x52 0 0 "Freeze the CPU"
      , Instruction SRE "IndirectY" 0x53 2 8 "LSR oper + EOR oper"
      , Instruction NOP "ZeroPageX" 0x54 2 4 "No operation"
      , Instruction EOR "ZeroPageX" 0x55 2 4 "Exclusive-OR Memory with Accumulator"
      , Instruction LSR "ZeroPageX" 0x56 2 6 "Shift One Bit Right"
      , Instruction SRE "ZeroPageX" 0x57 2 6 "LSR oper + EOR oper"
      , Instruction CLI "Implied" 0x58 1 2 "Clear Interrupr Disable Bit"
      , Instruction EOR "AbsoluteY" 0x59 3 4 "Exclusive-OR Memory with Accumulator"
      , Instruction NOP "Implied" 0x5A 1 2 "No operation"
      , Instruction SRE "AbsoluteY" 0x5B 3 7 "LSR oper + EOR oper"
      , Instruction NOP "AbsoluteX" 0x5C 3 4 "No operation"
      , Instruction EOR "AbsoluteX" 0x5D 3 4 "Exclusive-OR Memory with Accumulator"
      , Instruction LSR "AbsoluteX" 0x5E 3 7 "Shift One Bit Right"
      , Instruction SRE "AbsoluteX" 0x5F 3 7 "LSR oper + EOR oper"
      , Instruction RTS "Implied" 0x60 1 6 "Return from Subroutine"
      , Instruction ADC "IndirectX" 0x61 2 6 "Add Memory to Accumulator with Carry"
      , Instruction JAM "Implied" 0x62 0 0 "Freeze the CPU"
      , Instruction RRA "IndirectX" 0x63 2 8 "ROR oper + ADC oper"
      , Instruction NOP "ZeroPage" 0x64 2 3 "No operation"
      , Instruction ADC "ZeroPage" 0x65 2 3 "Add Memory to Accumulator with Carry"
      , Instruction ROR "ZeroPage" 0x66 2 5 "Rotate One Bit Right"
      , Instruction RRA "ZeroPage" 0x67 2 5 "ROR oper + ADC oper"
      , Instruction PLA "Implied" 0x68 1 4 "Pull Accumulator from Stack"
      , Instruction ADC "Immediate" 0x69 2 2 "Add Memory to Accumulator with Carry"
      , Instruction ROR "Accumulator" 0x6A 1 2 "Rotate One Bit Right"
      , Instruction ARR "Immediate" 0x6B 2 2 "AND oper + ROR"
      , Instruction JMP "Indirect" 0x6C 3 5 "Jump to New Location"
      , Instruction ADC "Absolute" 0x6D 3 4 "Add Memory to Accumulator with Carry"
      , Instruction ROR "Absolute" 0x6E 3 6 "Rotate One Bit Right"
      , Instruction RRA "Absolute" 0x6F 3 6 "ROR oper + ADC oper"
      , Instruction BVS "Relative" 0x70 2 2 "Branch on Overflow Set"
      , Instruction ADC "IndirectY" 0x71 2 5 "Add Memory to Accumulator with Carry"
      , Instruction JAM "Implied" 0x72 0 0 "Freeze the CPU"
      , Instruction RRA "IndirectY" 0x73 2 8 "ROR oper + ADC oper"
      , Instruction NOP "ZeroPageX" 0x74 2 4 "No operation"
      , Instruction ADC "ZeroPageX" 0x75 2 4 "Add Memory to Accumulator with Carry"
      , Instruction ROR "ZeroPageX" 0x76 2 6 "Rotate One Bit Right"
      , Instruction RRA "ZeroPageX" 0x77 2 6 "ROR oper + ADC oper"
      , Instruction SEI "Implied" 0x78 1 2 "Set Interrupte Disable Status"
      , Instruction ADC "AbsoluteY" 0x79 3 4 "Add Memory to Accumulator with Carry"
      , Instruction NOP "Implied" 0x7A 1 2 "No operation"
      , Instruction RRA "AbsoluteY" 0x7B 3 7 "ROR oper + ADC oper"
      , Instruction NOP "AbsoluteX" 0x7C 3 4 "No operation"
      , Instruction ADC "AbsoluteX" 0x7D 3 4 "Add Memory to Accumulator with Carry"
      , Instruction ROR "AbsoluteX" 0x7E 3 7 "Rotate One Bit Right"
      , Instruction RRA "AbsoluteX" 0x7F 3 7 "ROR oper + ADC oper"
      , Instruction NOP "Immediate" 0x80 2 2 "No operation"
      , Instruction STA "IndirectX" 0x81 2 6 "Store Accumulator in Memory"
      , Instruction NOP "Immediate" 0x82 2 2 "No operation"
      , Instruction SAX "IndirectX" 0x83 2 6 "A AND X -> Memory"
      , Instruction STY "ZeroPage" 0x84 2 3 "Store Y in Memory"
      , Instruction STA "ZeroPage" 0x85 2 3 "Store Accumulator in Memory"
      , Instruction STX "ZeroPage" 0x86 2 3 "Store X In Memory"
      , Instruction SAX "ZeroPage" 0x87 2 3 "A AND X -> Memory"
      , Instruction DEY "Implied" 0x88 1 2 "Decrement Y"
      , Instruction NOP "Immediate" 0x89 2 2 "No operation"
      , Instruction TXA "Implied" 0x8A 1 2 "Transfer X to Accumulator"
      , Instruction ANE "Immediate" 0x8B 2 2 "OR X + AND oper, HIGHLY UNSTABLE"
      , Instruction STY "Absolute" 0x8C 3 4 "Store Y in Memory"
      , Instruction STA "Absolute" 0x8D 3 4 "Store Accumulator in Memory"
      , Instruction STX "Absolute" 0x8E 3 4 "Store X In Memory"
      , Instruction SAX "Absolute" 0x8F 3 4 "A AND X -> Memory"
      , Instruction BCC "Relative" 0x90 2 2 "Branch on Carry Clear"
      , Instruction STA "IndirectY" 0x91 2 6 "Store Accumulator in Memory"
      , Instruction JAM "Implied" 0x92 0 0 "Freeze the CPU"
      , Instruction SHA "IndirectY" 0x93 2 6 "A AND X AND (H+1) -> M, UNSTABLE"
      , Instruction STY "ZeroPageX" 0x94 2 4 "Store Y in Memory"
      , Instruction STA "ZeroPageX" 0x95 2 4 "Store Accumulator in Memory"
      , Instruction STX "ZeroPageY" 0x96 2 4 "Store X In Memory"
      , Instruction SAX "ZeroPageY" 0x97 2 4 "A AND X -> Memory"
      , Instruction TYA "Implied" 0x98 1 2 "Transfer Y to Accumulator"
      , Instruction STA "AbsoluteY" 0x99 3 5 "Store Accumulator in Memory"
      , Instruction TXS "Implied" 0x9A 1 2 "Transfer X to Stack Register"
      , Instruction TAS "AbsoluteY" 0x9B 3 5 "A AND X -> SP, A AND X AND (H+1) -> M, UNSTABLE"
      , Instruction SHY "AbsoluteX" 0x9C 3 5 "Y AND (H+1) -> M, UNSTABLE"
      , Instruction STA "AbsoluteX" 0x9D 3 5 "Store Accumulator in Memory"
      , Instruction SHA "AbsoluteY" 0x9F 3 5 "A AND X AND (H+1) -> M, UNSTABLE"
      , Instruction LDY "Immediate" 0xA0 2 2 "Load Y with Memory"
      , Instruction LDA "IndirectX" 0xA1 2 6 "Load Accumulator with Memory"
      , Instruction LDX "Immediate" 0xA2 2 2 "Load X with Memory"
      , Instruction LAX "IndirectX" 0xA3 2 6 "LDA oper + LDX oper"
      , Instruction LDY "ZeroPage" 0xA4 2 3 "Load Y into Memory"
      , Instruction LDA "ZeroPage" 0xA5 2 3 "Load Accumulator with Memory"
      , Instruction LDX "ZeroPage" 0xA6 2 3 "Load X with Memory"
      , Instruction LAX "ZeroPage" 0xA7 2 3 "LDA oper + LDX oper"
      , Instruction TAY "Implied" 0xA8 1 2 "Transfer Accumulator to Y"
      , Instruction LDA "Immediate" 0xA9 2 2 "Load Accumulator with Memory"
      , Instruction TAX "Implied" 0xAA 1 2 "Transfer Accumulator to A"
      , Instruction LXA "Immediate" 0xAB 2 2 "Store * AND oper in A and X, HIGHLY UNSTABLE"
      , Instruction LDY "Absolute" 0xAC 3 4 "Load Y into Memory"
      , Instruction LDA "Absolute" 0xAD 3 4 "Load Accumulator with Memory"
      , Instruction LDX "Absolute" 0xAE 3 4 "Load X with Memory"
      , Instruction LAX "Absolute" 0xAF 3 4 "LDA oper + LDX oper"
      , Instruction BCS "Relative" 0xB0 2 2 "Branch on Carry Set"
      , Instruction LDA "IndirectY" 0xB1 2 5 "Load Accumulator with Memory"
      , Instruction JAM "Implied" 0xB2 0 0 "Freeze the CPU"
      , Instruction LAX "IndirectY" 0xB3 2 5 "LDA oper + LDX oper"
      , Instruction LDY "ZeroPageX" 0xB4 2 4 "Load Y into Memory"
      , Instruction LDA "ZeroPageX" 0xB5 2 4 "Load Accumulator with Memory"
      , Instruction LDX "ZeroPageY" 0xB6 2 4 "Load X with Memory"
      , Instruction LAX "ZeroPageY" 0xB7 2 4 "LDA oper + LDX oper"
      , Instruction CLV "Implied" 0xB8 1 2 "Clear Overflow Flag"
      , Instruction LDA "AbsoluteY" 0xB9 3 4 "Load Accumulator with Memory"
      , Instruction TSX "Implied" 0xBA 1 2 "Transfer Stack Pointer to X"
      , Instruction LAS "AbsoluteY" 0xBB 3 4 "LDA/TSX oper"
      , Instruction LDY "AbsoluteX" 0xBC 3 4 "Load Y into Memory"
      , Instruction LDA "AbsoluteX" 0xBD 3 4 "Load Accumulator with Memory"
      , Instruction LDX "AbsoluteY" 0xBE 3 4 "Load X with Memory"
      , Instruction LAX "AbsoluteY" 0xBF 3 4 "LDA oper + LDX oper"
      , Instruction CPY "Immediate" 0xC0 2 2 "Compare Memory and Y"
      , Instruction CMP "IndirectX" 0xC1 2 6 "Compare Memory with Accumulator"
      , Instruction NOP "Immediate" 0xC2 2 2 "No operation"
      , Instruction DCP "IndirectX" 0xC3 2 8 "DEC oper + CMP oper"
      , Instruction CPY "ZeroPage" 0xC4 2 3 "Compare Memory and Y"
      , Instruction CMP "ZeroPage" 0xC5 2 3 "Compare Memory with Accumulator"
      , Instruction DEC "ZeroPage" 0xC6 2 5 "Decrement Memory by One"
      , Instruction DCP "ZeroPage" 0xC7 2 5 "DEC oper + CMP oper"
      , Instruction INY "Implied" 0xC8 1 2 "Increment Y by One"
      , Instruction CMP "Immediate" 0xC9 2 2 "Compare Memory with Accumulator"
      , Instruction DEX "Implied" 0xCA 1 2 "Decrement X by One"
      , Instruction SBX "Immediate" 0xCB 2 2 "CMP oper + DEX oper"
      , Instruction CPY "Absolute" 0xCC 3 4 "Compare Memory and Y"
      , Instruction CMP "Absolute" 0xCD 3 4 "Compare Memory with Accumulator"
      , Instruction DEC "Absolute" 0xCE 3 6 "Decrement Memory by One"
      , Instruction DCP "Absolute" 0xCF 3 6 "DEC oper + CMP oper"
      , Instruction BNE "Relative" 0xD0 2 2 "Branch on Result not Zero"
      , Instruction CMP "IndirectY" 0xD1 2 5 "Compare Memory with Accumulator"
      , Instruction JAM "Implied" 0xD2 0 0 "Freeze the CPU"
      , Instruction DCP "IndirectY" 0xD3 2 8 "DEC oper + CMP oper"
      , Instruction NOP "ZeroPageX" 0xD4 2 4 "No operation"
      , Instruction CMP "ZeroPageX" 0xD5 2 4 "Compare Memory with Accumulator"
      , Instruction DEC "ZeroPageX" 0xD6 2 6 "Decrement Memory by One"
      , Instruction DCP "ZeroPageX" 0xD7 2 6 "DEC oper + CMP oper"
      , Instruction CLD "Implied" 0xD8 1 2 "Clear Decimal Mode"
      , Instruction CMP "AbsoluteY" 0xD9 3 4 "Compare Memory with Accumulator"
      , Instruction NOP "Implied" 0xDA 1 2 "No operation"
      , Instruction DCP "AbsoluteY" 0xDB 3 7 "DEC oper + CMP oper"
      , Instruction NOP "AbsoluteX" 0xDC 3 4 "No operation"
      , Instruction CMP "AbsoluteX" 0xDD 3 4 "Compare Memory with Accumulator"
      , Instruction DEC "AbsoluteX" 0xDE 3 7 "Decrement Memory by One"
      , Instruction DCP "AbsoluteX" 0xDF 3 7 "DEC oper + CMP oper"
      , Instruction CPX "Immediate" 0xE0 2 2 "Compare Memory and X"
      , Instruction SBC "IndirectX" 0xE1 2 6 "Subtract Memory from Accumulator with Borrow"
      , Instruction NOP "Immediate" 0xE2 2 2 "No operation"
      , Instruction ISC "IndirectX" 0xE3 2 8 "INC oper + CMP oper"
      , Instruction CPX "ZeroPage" 0xE4 2 3 "Compare Memory and X"
      , Instruction SBC "ZeroPage" 0xE5 2 3 "Subtract Memory from Accumulator with Borrow"
      , Instruction INC "ZeroPage" 0xE6 2 5 "Increment Memory by One"
      , Instruction ISC "ZeroPage" 0xE7 2 5 "INC oper + CMP oper"
      , Instruction INX "Implied" 0xE8 1 2 "Increment X by One"
      , Instruction SBC "Immediate" 0xE9 2 2 "Subtract Memory from Accumulator with Borrow"
      , Instruction NOP "Immediate" 0xEA 1 2 "No operation"
      , Instruction UBC "Immediate" 0xEB 2 2 "SBC oper + NOP"
      , Instruction CPX "Absolute" 0xEC 3 4 "Compare Memory and X"
      , Instruction SBC "Absolute" 0xED 3 4 "Subtract Memory from Accumulator with Borrow"
      , Instruction INC "Absolute" 0xEE 3 6 "Increment Memory by One"
      , Instruction ISC "Absolute" 0xEF 3 6 "INC oper + CMP oper"
      , Instruction BEQ "Relative" 0xF0 2 2 "Branch on Result Zero"
      , Instruction SBC "IndirectY" 0xF1 2 5 "Subtract Memory from Accumulator with Borrow"
      , Instruction JAM "Implied" 0xF2 0 0 "Freeze the CPU"
      , Instruction ISC "IndirectY" 0xF3 2 8 "INC oper + CMP oper"
      , Instruction NOP "ZeroPageX" 0xF4 2 4 "No operation"
      , Instruction SBC "ZeroPageX" 0xF5 2 4 "Subtract Memory from Accumulator with Borrow"
      , Instruction INC "ZeroPageX" 0xF6 2 6 "Increment Memory by One"
      , Instruction ISC "ZeroPageX" 0xF7 2 6 "INC oper + CMP oper"
      , Instruction SED "Implied" 0xF8 1 2 "Set Decimal Flag"
      , Instruction SBC "AbsoluteY" 0xF9 3 4 "Subtract Memory from Accumulator with Borrow"
      , Instruction NOP "Implied" 0xFA 1 2 "No operation"
      , Instruction ISC "AbsoluteY" 0xFB 3 7 "INC oper + CMP oper"
      , Instruction NOP "AbsoluteX" 0xFC 3 4 "No operation"
      , Instruction SBC "AbsoluteX" 0xFD 3 4 "Subtract Memory from Accumulator with Borrow"
      , Instruction INC "AbsoluteX" 0xFE 3 7 "Increment Memory by One"
      , Instruction ISC "AbsoluteX" 0xFF 3 7 "INC oper + CMP oper"
      ]

jamInstruction :: Instruction
jamInstruction = Instruction JAM "Implied" 0x02 0 0 "Freeze the CPU"

-- Something that might be able to be done is bounds-checking as a CONSTRAINT, but prolly not.

-- This mess is commented out because it is fundamentally flawed.
-- In order to get some functionality to the type-level, such as bounds-checking
-- or instruction validation, we would need to put the whole emulator at the type level.
-- Which is not something I want to do. The syntax is bad enough.

-- type Atari i j = RWIPT Env () IO i j ()
-- data MachineState (bytes :: Nat) (cycles :: Nat) = MachineState

-- type family
--   Combine
--     (i :: MachineState (b1 :: Nat) (c1 :: Nat))
--     (j :: MachineState (b2 :: Nat) (c2 :: Nat))
--     :: k
--   where
--   Combine (i :: MachineState b1 c1) (j :: MachineState b2 c2) =
--     MachineState (b1 + c1) (b2 + c2)

-- data Env = Env

-- asl :: forall i i2 r . String -> (forall j j2 . Atari (MachineState i i2) (MachineState j j2) -> r) -> r
-- asl operand fn =
--   case operand of
--     "" -> fn a23
--     _ -> fn a64

-- a23 :: Atari (MachineState i j) (MachineState (i + 2) (i + 3))
-- a23 = undefined

-- a64 :: Atari (MachineState i j) (MachineState (i + 6) (i + 3))
-- a64 = undefined
--
{-
Address Range | Function
\$0000 - $007F | TIA registers (only 00-2F are used)
\$0080 - $00FF | RAM
\$0200 - $02FF | RIOT registers (only 280-297 are used)
\$F000 - $FFFF | ROM
-}
