{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Korigatachi.Atari.Model where

-- containers
import Data.Map.Strict qualified as Map

-- finite-typelits
import Data.Finite (finite)

-- text
import Data.Text qualified as T

-- sized-vector
import Data.Vector.Sized ((//))
import Data.Vector.Sized qualified as Sized

-- base

import Data.Bits (Bits (bit, (.|.)), testBit)
import Data.List as List
import Data.Word (Word8)
import Prelude hiding (break)

-- korigatachi
import Korigatachi.Types

-- | An Atari with nothing. Every register is 0'd out.
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
          (rom4k.memory4k // [(finite $ fromIntegral rom4k.focus, w8)])
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
flagsToWord8
  Flags
    { negative
    , overflow
    , ignored
    , break
    , decimal
    , interrupt
    , zero
    , carry
    } =
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

allInstructions :: Map.Map Shorthand [Instruction]
allInstructions =
  Map.fromListWith (flip (++)) $
    (\ins -> (shorthand ins, [ins])) <$> (instructions <$> [0x00 .. 0xFF])

instructions :: Word8 -> Instruction
instructions 0x00 = Instruction BRK "Implied" 0x00 1 7 "Force Break"
instructions 0x01 = Instruction ORA "IndirectX" 0x01 2 6 "OR Memory with Accumulator"
instructions 0x02 = Instruction JAM "Implied" 0x02 0 0 "Freeze the CPU"
instructions 0x03 = Instruction SLO "IndirectX" 0x03 2 8 "ASL op + ORA op"
instructions 0x04 = Instruction NOP "ZeroPage" 0x04 2 3 "No operation"
instructions 0x05 = Instruction ORA "ZeroPage" 0x05 2 3 "OR Memory with Accumulator"
instructions 0x06 = Instruction ASL "ZeroPage" 0x06 2 5 "Shift Left One Bit"
instructions 0x07 = Instruction SLO "ZeroPage" 0x07 2 5 "ASL op + ORA op"
instructions 0x08 = Instruction PHP "Implied" 0x08 1 3 "Push Processor Status on Stack"
instructions 0x09 = Instruction ORA "Immediate" 0x09 2 2 "OR Memory with Accumulator"
instructions 0x0A = Instruction ASL "Accumulator" 0x0A 1 2 "Shift Left One Bit"
instructions 0x0B = Instruction ANC "Immediate" 0x0B 2 2 "AND oper + set C as ASL"
instructions 0x0C = Instruction NOP "Absolute" 0x0C 3 4 "No operation"
instructions 0x0D = Instruction ORA "Absolute" 0x0D 3 4 "OR Memory with Accumulator"
instructions 0x0E = Instruction ASL "Absolute" 0x0E 3 6 "Shift Left One Bit"
instructions 0x0F = Instruction SLO "Absolute" 0x0F 3 6 "ASL op + ORA op"
instructions 0x10 = Instruction BPL "Relative" 0x10 2 2 "Branch on N = 0"
instructions 0x11 = Instruction ORA "IndirectY" 0x11 2 5 "OR Memory with Accumulator"
instructions 0x12 = Instruction JAM "Implied" 0x12 0 0 "Freeze the CPU"
instructions 0x13 = Instruction SLO "IndirectY" 0x13 2 8 "ASL op + ORA op"
instructions 0x14 = Instruction NOP "ZeroPageX" 0x14 2 4 "No operation"
instructions 0x15 = Instruction ORA "ZeroPageX" 0x15 2 4 "OR Memory with Accumulator"
instructions 0x16 = Instruction ASL "ZeroPageX" 0x16 2 6 "Shift Left One Bit"
instructions 0x17 = Instruction SLO "ZeroPageX" 0x17 2 5 "ASL op + ORA op"
instructions 0x18 = Instruction CLC "Implied" 0x18 1 2 "Clear Carry Flag"
instructions 0x19 = Instruction ORA "AbsoluteY" 0x19 3 4 "OR Memory with Accumulator"
instructions 0x1A = Instruction NOP "Implied" 0x1A 1 2 "No operation"
instructions 0x1B = Instruction SLO "AbsoluteY" 0x1B 3 7 "ASL op + ORA op"
instructions 0x1C = Instruction NOP "AbsoluteX" 0x1C 1 2 "No operation"
instructions 0x1D = Instruction ORA "AbsoluteX" 0x1D 3 4 "OR Memory with Accumulator"
instructions 0x1E = Instruction ASL "AbsoluteX" 0x1E 3 7 "Shift Left One Bit"
instructions 0x1F = Instruction SLO "AbsoluteX" 0x1F 3 7 "ASL oper + ORA oper"
instructions 0x20 = Instruction JSR "Absolute" 0x20 3 6 "Jump to subroutine"
instructions 0x21 = Instruction AND "IndirectX" 0x21 2 6 "AND memory with accumulator"
instructions 0x22 = Instruction JAM "Implied" 0x22 0 0 "Freeze the CPU"
instructions 0x23 = Instruction RLA "IndirectX" 0x23 2 8 "ROL oper + AND oper"
instructions 0x24 = Instruction BIT "ZeroPage" 0x24 2 3 "Test Bits in Memory with accumulator"
instructions 0x25 = Instruction AND "ZeroPage" 0x25 2 3 "AND Memory with Accumulator"
instructions 0x26 = Instruction ROL "ZeroPage" 0x26 2 5 "Rotate One Bit Left"
instructions 0x27 = Instruction RLA "ZeroPage" 0x27 2 5 "ROL oper + AND oper"
instructions 0x28 = Instruction PLP "Implied" 0x28 1 4 "Pull processor status from stack"
instructions 0x29 = Instruction AND "Immediate" 0x29 2 2 "AND Memory with Accumulator"
instructions 0x2A = Instruction ROL "Accumulator" 0x2A 1 2 "Rotate One Bit Left"
instructions 0x2B = Instruction ANC "Immediate" 0x2B 2 2 "AND oper + set C as ASL"
instructions 0x2C = Instruction BIT "Absolute" 0x2C 3 4 "Test Bits in Memory with accumulator"
instructions 0x2D = Instruction AND "Absolute" 0x2D 3 4 "AND memory with accumulator"
instructions 0x2E = Instruction ROL "Absolute" 0x2E 3 6 "Rotate One Bit Left"
instructions 0x2F = Instruction RLA "Absolute" 0x2F 3 6 "ROL oper + AND oper"
instructions 0x30 = Instruction BMI "Relative" 0x30 2 2 "Branch on Result Minus"
instructions 0x31 = Instruction AND "IndirectY" 0x31 2 5 "AND Memory with Accumulator"
instructions 0x32 = Instruction JAM "Implied" 0x32 0 0 "Freeze the CPU"
instructions 0x33 = Instruction RLA "IndirectY" 0x33 2 8 "ROL Oper + AND oper"
instructions 0x34 = Instruction NOP "ZeroPageX" 0x34 2 4 "No operation"
instructions 0x35 = Instruction AND "ZeroPageX" 0x35 2 4 "AND Memory with Accumulator"
instructions 0x36 = Instruction ROL "ZeroPageX" 0x36 2 6 "Rotate One Bit Left"
instructions 0x37 = Instruction RLA "ZeroPageX" 0x37 2 6 "ROL Oper + AND oper"
instructions 0x38 = Instruction SEC "Implied" 0x38 1 2 "Set Carry Flag"
instructions 0x39 = Instruction AND "AbsoluteY" 0x39 3 4 "AND Memory with Accumulator"
instructions 0x3A = Instruction NOP "Implied" 0x3A 1 2 "No operation"
instructions 0x3B = Instruction RLA "AbsoluteY" 0x3B 3 7 "ROL Oper + AND oper"
instructions 0x3C = Instruction NOP "AbsoluteX" 0x3C 3 4 "No operation"
instructions 0x3D = Instruction AND "AbsoluteX" 0x3D 3 4 "AND Memory with Accumulator"
instructions 0x3E = Instruction ROL "AbsoluteX" 0x3E 3 7 "Rotate One Bit Left"
instructions 0x3F = Instruction RLA "AbsoluteX" 0x3F 3 7 "ROL Oper + AND oper"
instructions 0x40 = Instruction RTI "Implied" 0x40 1 6 "Return from Interrupt"
instructions 0x41 = Instruction EOR "IndirectX" 0x41 2 6 "Exclusive-OR Memory with Accumulator"
instructions 0x42 = Instruction JAM "Implied" 0x42 0 0 "Freeze the CPU"
instructions 0x43 = Instruction SRE "IndirectX" 0x43 2 8 "LSR oper + EOR oper"
instructions 0x44 = Instruction NOP "ZeroPage" 0x44 2 3 "No operation"
instructions 0x45 = Instruction EOR "ZeroPage" 0x45 2 3 "Exclusive-OR Memory with Accumulator"
instructions 0x46 = Instruction LSR "ZeroPage" 0x46 2 5 "Shift One Bit Right"
instructions 0x47 = Instruction SRE "ZeroPage" 0x47 2 5 "LSR oper + EOR oper"
instructions 0x48 = Instruction PHA "Implied" 0x48 1 3 "Push Accumulator on Stack"
instructions 0x49 = Instruction EOR "Immediate" 0x49 2 2 "Exclusive-OR Memory with Accumulator"
instructions 0x4A = Instruction LSR "Accumulator" 0x4A 1 2 "Shift One Bit Right"
instructions 0x4B = Instruction ALR "Immediate" 0x4B 2 2 "AND oper + LSR"
instructions 0x4C = Instruction JMP "Absolute" 0x4C 3 3 "Jump to New Location"
instructions 0x4D = Instruction EOR "Absolute" 0x4D 3 4 "Exclusive-OR Memory with Accumulator"
instructions 0x4E = Instruction LSR "Absolute" 0x4E 3 6 "Shift One Bit Right"
instructions 0x4F = Instruction SRE "Absolute" 0x4F 3 6 "LSR oper + EOR oper"
instructions 0x50 = Instruction BVC "Relative" 0x50 2 2 "Branch on Overflow Clear"
instructions 0x51 = Instruction EOR "IndirectY" 0x51 2 5 "Exclusive-OR Memory with Accumulator"
instructions 0x52 = Instruction JAM "Implied" 0x52 0 0 "Freeze the CPU"
instructions 0x53 = Instruction SRE "IndirectY" 0x53 2 8 "LSR oper + EOR oper"
instructions 0x54 = Instruction NOP "ZeroPageX" 0x54 2 4 "No operation"
instructions 0x55 = Instruction EOR "ZeroPageX" 0x55 2 4 "Exclusive-OR Memory with Accumulator"
instructions 0x56 = Instruction LSR "ZeroPageX" 0x56 2 6 "Shift One Bit Right"
instructions 0x57 = Instruction SRE "ZeroPageX" 0x57 2 6 "LSR oper + EOR oper"
instructions 0x58 = Instruction CLI "Implied" 0x58 1 2 "Clear Interrupr Disable Bit"
instructions 0x59 = Instruction EOR "AbsoluteY" 0x59 3 4 "Exclusive-OR Memory with Accumulator"
instructions 0x5A = Instruction NOP "Implied" 0x5A 1 2 "No operation"
instructions 0x5B = Instruction SRE "AbsoluteY" 0x5B 3 7 "LSR oper + EOR oper"
instructions 0x5C = Instruction NOP "AbsoluteX" 0x5C 3 4 "No operation"
instructions 0x5D = Instruction EOR "AbsoluteX" 0x5D 3 4 "Exclusive-OR Memory with Accumulator"
instructions 0x5E = Instruction LSR "AbsoluteX" 0x5E 3 7 "Shift One Bit Right"
instructions 0x5F = Instruction SRE "AbsoluteX" 0x5F 3 7 "LSR oper + EOR oper"
instructions 0x60 = Instruction RTS "Implied" 0x60 1 6 "Return from Subroutine"
instructions 0x61 = Instruction ADC "IndirectX" 0x61 2 6 "Add Memory to Accumulator with Carry"
instructions 0x62 = Instruction JAM "Implied" 0x62 0 0 "Freeze the CPU"
instructions 0x63 = Instruction RRA "IndirectX" 0x63 2 8 "ROR oper + ADC oper"
instructions 0x64 = Instruction NOP "ZeroPage" 0x64 2 3 "No operation"
instructions 0x65 = Instruction ADC "ZeroPage" 0x65 2 3 "Add Memory to Accumulator with Carry"
instructions 0x66 = Instruction ROR "ZeroPage" 0x66 2 5 "Rotate One Bit Right"
instructions 0x67 = Instruction RRA "ZeroPage" 0x67 2 5 "ROR oper + ADC oper"
instructions 0x68 = Instruction PLA "Implied" 0x68 1 4 "Pull Accumulator from Stack"
instructions 0x69 = Instruction ADC "Immediate" 0x69 2 2 "Add Memory to Accumulator with Carry"
instructions 0x6A = Instruction ROR "Accumulator" 0x6A 1 2 "Rotate One Bit Right"
instructions 0x6B = Instruction ARR "Immediate" 0x6B 2 2 "AND oper + ROR"
instructions 0x6C = Instruction JMP "Indirect" 0x6C 3 5 "Jump to New Location"
instructions 0x6D = Instruction ADC "Absolute" 0x6D 3 4 "Add Memory to Accumulator with Carry"
instructions 0x6E = Instruction ROR "Absolute" 0x6E 3 6 "Rotate One Bit Right"
instructions 0x6F = Instruction RRA "Absolute" 0x6F 3 6 "ROR oper + ADC oper"
instructions 0x70 = Instruction BVS "Relative" 0x70 2 2 "Branch on Overflow Set"
instructions 0x71 = Instruction ADC "IndirectY" 0x71 2 5 "Add Memory to Accumulator with Carry"
instructions 0x72 = Instruction JAM "Implied" 0x72 0 0 "Freeze the CPU"
instructions 0x73 = Instruction RRA "IndirectY" 0x73 2 8 "ROR oper + ADC oper"
instructions 0x74 = Instruction NOP "ZeroPageX" 0x74 2 4 "No operation"
instructions 0x75 = Instruction ADC "ZeroPageX" 0x75 2 4 "Add Memory to Accumulator with Carry"
instructions 0x76 = Instruction ROR "ZeroPageX" 0x76 2 6 "Rotate One Bit Right"
instructions 0x77 = Instruction RRA "ZeroPageX" 0x77 2 6 "ROR oper + ADC oper"
instructions 0x78 = Instruction SEI "Implied" 0x78 1 2 "Set Interrupte Disable Status"
instructions 0x79 = Instruction ADC "AbsoluteY" 0x79 3 4 "Add Memory to Accumulator with Carry"
instructions 0x7A = Instruction NOP "Implied" 0x7A 1 2 "No operation"
instructions 0x7B = Instruction RRA "AbsoluteY" 0x7B 3 7 "ROR oper + ADC oper"
instructions 0x7C = Instruction NOP "AbsoluteX" 0x7C 3 4 "No operation"
instructions 0x7D = Instruction ADC "AbsoluteX" 0x7D 3 4 "Add Memory to Accumulator with Carry"
instructions 0x7E = Instruction ROR "AbsoluteX" 0x7E 3 7 "Rotate One Bit Right"
instructions 0x7F = Instruction RRA "AbsoluteX" 0x7F 3 7 "ROR oper + ADC oper"
instructions 0x80 = Instruction NOP "Immediate" 0x80 2 2 "No operation"
instructions 0x81 = Instruction STA "IndirectX" 0x81 2 6 "Store Accumulator in Memory"
instructions 0x82 = Instruction NOP "Immediate" 0x82 2 2 "No operation"
instructions 0x83 = Instruction SAX "IndirectX" 0x83 2 6 "A AND X -> Memory"
instructions 0x84 = Instruction STY "ZeroPage" 0x84 2 3 "Store Y in Memory"
instructions 0x85 = Instruction STA "ZeroPage" 0x85 2 3 "Store Accumulator in Memory"
instructions 0x86 = Instruction STX "ZeroPage" 0x86 2 3 "Store X In Memory"
instructions 0x87 = Instruction SAX "ZeroPage" 0x87 2 3 "A AND X -> Memory"
instructions 0x88 = Instruction DEY "Implied" 0x88 1 2 "Decrement Y"
instructions 0x89 = Instruction NOP "Immediate" 0x89 2 2 "No operation"
instructions 0x8A = Instruction TXA "Implied" 0x8A 1 2 "Transfer X to Accumulator"
instructions 0x8B = Instruction ANE "Immediate" 0x8B 2 2 "OR X + AND oper, HIGHLY UNSTABLE"
instructions 0x8C = Instruction STY "Absolute" 0x8C 3 4 "Store Y in Memory"
instructions 0x8D = Instruction STA "Absolute" 0x8D 3 4 "Store Accumulator in Memory"
instructions 0x8E = Instruction STX "Absolute" 0x8E 3 4 "Store X In Memory"
instructions 0x8F = Instruction SAX "Absolute" 0x8F 3 4 "A AND X -> Memory"
instructions 0x90 = Instruction BCC "Relative" 0x90 2 2 "Branch on Carry Clear"
instructions 0x91 = Instruction STA "IndirectY" 0x91 2 6 "Store Accumulator in Memory"
instructions 0x92 = Instruction JAM "Implied" 0x92 0 0 "Freeze the CPU"
instructions 0x93 = Instruction SHA "IndirectY" 0x93 2 6 "A AND X AND (H+1) -> M, UNSTABLE"
instructions 0x94 = Instruction STY "ZeroPageX" 0x94 2 4 "Store Y in Memory"
instructions 0x95 = Instruction STA "ZeroPageX" 0x95 2 4 "Store Accumulator in Memory"
instructions 0x96 = Instruction STX "ZeroPageY" 0x96 2 4 "Store X In Memory"
instructions 0x97 = Instruction SAX "ZeroPageY" 0x97 2 4 "A AND X -> Memory"
instructions 0x98 = Instruction TYA "Implied" 0x98 1 2 "Transfer Y to Accumulator"
instructions 0x99 = Instruction STA "AbsoluteY" 0x99 3 5 "Store Accumulator in Memory"
instructions 0x9A = Instruction TXS "Implied" 0x9A 1 2 "Transfer X to Stack Register"
instructions 0x9B = Instruction TAS "AbsoluteY" 0x9B 3 5 "A AND X -> SP, A AND X AND (H+1) -> M, UNSTABLE"
instructions 0x9C = Instruction SHY "AbsoluteX" 0x9C 3 5 "Y AND (H+1) -> M, UNSTABLE"
instructions 0x9D = Instruction STA "AbsoluteX" 0x9D 3 5 "Store Accumulator in Memory"
instructions 0x9F = Instruction SHA "AbsoluteY" 0x9F 3 5 "A AND X AND (H+1) -> M, UNSTABLE"
instructions 0xA0 = Instruction LDY "Immediate" 0xA0 2 2 "Load Y with Memory"
instructions 0xA1 = Instruction LDA "IndirectX" 0xA1 2 6 "Load Accumulator with Memory"
instructions 0xA2 = Instruction LDX "Immediate" 0xA2 2 2 "Load X with Memory"
instructions 0xA3 = Instruction LAX "IndirectX" 0xA3 2 6 "LDA oper + LDX oper"
instructions 0xA4 = Instruction LDY "ZeroPage" 0xA4 2 3 "Load Y into Memory"
instructions 0xA5 = Instruction LDA "ZeroPage" 0xA5 2 3 "Load Accumulator with Memory"
instructions 0xA6 = Instruction LDX "ZeroPage" 0xA6 2 3 "Load X with Memory"
instructions 0xA7 = Instruction LAX "ZeroPage" 0xA7 2 3 "LDA oper + LDX oper"
instructions 0xA8 = Instruction TAY "Implied" 0xA8 1 2 "Transfer Accumulator to Y"
instructions 0xA9 = Instruction LDA "Immediate" 0xA9 2 2 "Load Accumulator with Memory"
instructions 0xAA = Instruction TAX "Implied" 0xAA 1 2 "Transfer Accumulator to A"
instructions 0xAB = Instruction LXA "Immediate" 0xAB 2 2 "Store * AND oper in A and X, HIGHLY UNSTABLE"
instructions 0xAC = Instruction LDY "Absolute" 0xAC 3 4 "Load Y into Memory"
instructions 0xAD = Instruction LDA "Absolute" 0xAD 3 4 "Load Accumulator with Memory"
instructions 0xAE = Instruction LDX "Absolute" 0xAE 3 4 "Load X with Memory"
instructions 0xAF = Instruction LAX "Absolute" 0xAF 3 4 "LDA oper + LDX oper"
instructions 0xB0 = Instruction BCS "Relative" 0xB0 2 2 "Branch on Carry Set"
instructions 0xB1 = Instruction LDA "IndirectY" 0xB1 2 5 "Load Accumulator with Memory"
instructions 0xB2 = Instruction JAM "Implied" 0xB2 0 0 "Freeze the CPU"
instructions 0xB3 = Instruction LAX "IndirectY" 0xB3 2 5 "LDA oper + LDX oper"
instructions 0xB4 = Instruction LDY "ZeroPageX" 0xB4 2 4 "Load Y into Memory"
instructions 0xB5 = Instruction LDA "ZeroPageX" 0xB5 2 4 "Load Accumulator with Memory"
instructions 0xB6 = Instruction LDX "ZeroPageY" 0xB6 2 4 "Load X with Memory"
instructions 0xB7 = Instruction LAX "ZeroPageY" 0xB7 2 4 "LDA oper + LDX oper"
instructions 0xB8 = Instruction CLV "Implied" 0xB8 1 2 "Clear Overflow Flag"
instructions 0xB9 = Instruction LDA "AbsoluteY" 0xB9 3 4 "Load Accumulator with Memory"
instructions 0xBA = Instruction TSX "Implied" 0xBA 1 2 "Transfer Stack Pointer to X"
instructions 0xBB = Instruction LAS "AbsoluteY" 0xBB 3 4 "LDA/TSX oper"
instructions 0xBC = Instruction LDY "AbsoluteX" 0xBC 3 4 "Load Y into Memory"
instructions 0xBD = Instruction LDA "AbsoluteX" 0xBD 3 4 "Load Accumulator with Memory"
instructions 0xBE = Instruction LDX "AbsoluteY" 0xBE 3 4 "Load X with Memory"
instructions 0xBF = Instruction LAX "AbsoluteY" 0xBF 3 4 "LDA oper + LDX oper"
instructions 0xC0 = Instruction CPY "Immediate" 0xC0 2 2 "Compare Memory and Y"
instructions 0xC1 = Instruction CMP "IndirectX" 0xC1 2 6 "Compare Memory with Accumulator"
instructions 0xC2 = Instruction NOP "Immediate" 0xC2 2 2 "No operation"
instructions 0xC3 = Instruction DCP "IndirectX" 0xC3 2 8 "DEC oper + CMP oper"
instructions 0xC4 = Instruction CPY "ZeroPage" 0xC4 2 3 "Compare Memory and Y"
instructions 0xC5 = Instruction CMP "ZeroPage" 0xC5 2 3 "Compare Memory with Accumulator"
instructions 0xC6 = Instruction DEC "ZeroPage" 0xC6 2 5 "Decrement Memory by One"
instructions 0xC7 = Instruction DCP "ZeroPage" 0xC7 2 5 "DEC oper + CMP oper"
instructions 0xC8 = Instruction INY "Implied" 0xC8 1 2 "Increment Y by One"
instructions 0xC9 = Instruction CMP "Immediate" 0xC9 2 2 "Compare Memory with Accumulator"
instructions 0xCA = Instruction DEX "Implied" 0xCA 1 2 "Decrement X by One"
instructions 0xCB = Instruction SBX "Immediate" 0xCB 2 2 "CMP oper + DEX oper"
instructions 0xCC = Instruction CPY "Absolute" 0xCC 3 4 "Compare Memory and Y"
instructions 0xCD = Instruction CMP "Absolute" 0xCD 3 4 "Compare Memory with Accumulator"
instructions 0xCE = Instruction DEC "Absolute" 0xCE 3 6 "Decrement Memory by One"
instructions 0xCF = Instruction DCP "Absolute" 0xCF 3 6 "DEC oper + CMP oper"
instructions 0xD0 = Instruction BNE "Relative" 0xD0 2 2 "Branch on Result not Zero"
instructions 0xD1 = Instruction CMP "IndirectY" 0xD1 2 5 "Compare Memory with Accumulator"
instructions 0xD2 = Instruction JAM "Implied" 0xD2 0 0 "Freeze the CPU"
instructions 0xD3 = Instruction DCP "IndirectY" 0xD3 2 8 "DEC oper + CMP oper"
instructions 0xD4 = Instruction NOP "ZeroPageX" 0xD4 2 4 "No operation"
instructions 0xD5 = Instruction CMP "ZeroPageX" 0xD5 2 4 "Compare Memory with Accumulator"
instructions 0xD6 = Instruction DEC "ZeroPageX" 0xD6 2 6 "Decrement Memory by One"
instructions 0xD7 = Instruction DCP "ZeroPageX" 0xD7 2 6 "DEC oper + CMP oper"
instructions 0xD8 = Instruction CLD "Implied" 0xD8 1 2 "Clear Decimal Mode"
instructions 0xD9 = Instruction CMP "AbsoluteY" 0xD9 3 4 "Compare Memory with Accumulator"
instructions 0xDA = Instruction NOP "Implied" 0xDA 1 2 "No operation"
instructions 0xDB = Instruction DCP "AbsoluteY" 0xDB 3 7 "DEC oper + CMP oper"
instructions 0xDC = Instruction NOP "AbsoluteX" 0xDC 3 4 "No operation"
instructions 0xDD = Instruction CMP "AbsoluteX" 0xDD 3 4 "Compare Memory with Accumulator"
instructions 0xDE = Instruction DEC "AbsoluteX" 0xDE 3 7 "Decrement Memory by One"
instructions 0xDF = Instruction DCP "AbsoluteX" 0xDF 3 7 "DEC oper + CMP oper"
instructions 0xE0 = Instruction CPX "Immediate" 0xE0 2 2 "Compare Memory and X"
instructions 0xE1 = Instruction SBC "IndirectX" 0xE1 2 6 "Subtract Memory from Accumulator with Borrow"
instructions 0xE2 = Instruction NOP "Immediate" 0xE2 2 2 "No operation"
instructions 0xE3 = Instruction ISC "IndirectX" 0xE3 2 8 "INC oper + CMP oper"
instructions 0xE4 = Instruction CPX "ZeroPage" 0xE4 2 3 "Compare Memory and X"
instructions 0xE5 = Instruction SBC "ZeroPage" 0xE5 2 3 "Subtract Memory from Accumulator with Borrow"
instructions 0xE6 = Instruction INC "ZeroPage" 0xE6 2 5 "Increment Memory by One"
instructions 0xE7 = Instruction ISC "ZeroPage" 0xE7 2 5 "INC oper + CMP oper"
instructions 0xE8 = Instruction INX "Implied" 0xE8 1 2 "Increment X by One"
instructions 0xE9 = Instruction SBC "Immediate" 0xE9 2 2 "Subtract Memory from Accumulator with Borrow"
instructions 0xEA = Instruction NOP "Immediate" 0xEA 1 2 "No operation"
instructions 0xEB = Instruction UBC "Immediate" 0xEB 2 2 "SBC oper + NOP"
instructions 0xEC = Instruction CPX "Absolute" 0xEC 3 4 "Compare Memory and X"
instructions 0xED = Instruction SBC "Absolute" 0xED 3 4 "Subtract Memory from Accumulator with Borrow"
instructions 0xEE = Instruction INC "Absolute" 0xEE 3 6 "Increment Memory by One"
instructions 0xEF = Instruction ISC "Absolute" 0xEF 3 6 "INC oper + CMP oper"
instructions 0xF0 = Instruction BEQ "Relative" 0xF0 2 2 "Branch on Result Zero"
instructions 0xF1 = Instruction SBC "IndirectY" 0xF1 2 5 "Subtract Memory from Accumulator with Borrow"
instructions 0xF2 = Instruction JAM "Implied" 0xF2 0 0 "Freeze the CPU"
instructions 0xF3 = Instruction ISC "IndirectY" 0xF3 2 8 "INC oper + CMP oper"
instructions 0xF4 = Instruction NOP "ZeroPageX" 0xF4 2 4 "No operation"
instructions 0xF5 = Instruction SBC "ZeroPageX" 0xF5 2 4 "Subtract Memory from Accumulator with Borrow"
instructions 0xF6 = Instruction INC "ZeroPageX" 0xF6 2 6 "Increment Memory by One"
instructions 0xF7 = Instruction ISC "ZeroPageX" 0xF7 2 6 "INC oper + CMP oper"
instructions 0xF8 = Instruction SED "Implied" 0xF8 1 2 "Set Decimal Flag"
instructions 0xF9 = Instruction SBC "AbsoluteY" 0xF9 3 4 "Subtract Memory from Accumulator with Borrow"
instructions 0xFA = Instruction NOP "Implied" 0xFA 1 2 "No operation"
instructions 0xFB = Instruction ISC "AbsoluteY" 0xFB 3 7 "INC oper + CMP oper"
instructions 0xFC = Instruction NOP "AbsoluteX" 0xFC 3 4 "No operation"
instructions 0xFD = Instruction SBC "AbsoluteX" 0xFD 3 4 "Subtract Memory from Accumulator with Borrow"
instructions 0xFE = Instruction INC "AbsoluteX" 0xFE 3 7 "Increment Memory by One"
instructions 0xFF = Instruction ISC "AbsoluteX" 0xFF 3 7 "INC oper + CMP oper"
instructions _ = jamInstruction

jamInstruction :: Instruction
jamInstruction = Instruction JAM "Implied" 0x02 0 0 "Freeze the CPU"

-- fmap (\(x:xs) -> (shorthand x, xs))
--   . List.groupBy (\a b -> (shorthand a) == (shorthand b))
--   . List.sortBy (\a b -> shorthand a `compare` shorthand b)
--   $ instructions <$> [0x00..0xFF]

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
