{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.Word (Word16, Word8)
import GHC.Generics qualified as Generic

import Korigatachi.Monad

-- | From the Japanese for "congealed shape". The monad to rule them all.
type Korigatachi a = RWIT Env Katteyomi IO Atari Atari a

-- | From the Japanese for "selfish reading". The data structure for the writer.
data Katteyomi = Katteyomi
  { logs :: T.Text
  , codegen :: T.Text
  }
  deriving (Generic.Generic)

instance Semigroup Katteyomi where
  k1 <> k2 = Katteyomi (logs k1 <> logs k2) (codegen k1 <> codegen k2)

instance Monoid Katteyomi where
  mempty = Katteyomi "" ""

-- | Write to the writer.
katteyomi :: T.Text -> T.Text -> Korigatachi ()
katteyomi logMsg codeGen = tell (Katteyomi logMsg codeGen)

data ProgramState =
  Running Atari |
  Jammed Atari |
  Error KorigatachiError |
  Stopped

data KorigatachiError

data Atari = Atari
  { ram :: Memory
  -- ^ A whopping 128 bytes of RAM.
  , cpu :: MOS6507
  -- ^ A 6502 with the last 4 pins cut off. Costs $12 in 1977 money.
  , rom :: Memory4K
  -- ^ Read-only memory.
  , tv :: TV
  -- ^ The TV your Atari is plugged into.
  , tia :: TIA
  -- ^ Television Interface Adapter.
  , pia :: PIA
  {- ^ Off-the-shelf 6532 Peripheral Interface Adapter.
  Technically, the RAM is controlled by this chip, but
  it's easier to abstract the RAM to a seperate field.
  -}
  }
  deriving (Generic.Generic)

{- | The Atari 2600 only had 128 bytes of RAM.
Frankly, I'm stunned it even has that much.
We can't even use some of these, maybe some type level
synonym is in order that has a list of fields we can't use.
-}
type Memory = Sized.Vector 128 Word8

updateMemory :: Memory -> Word8 -> Word8 -> Either T.Text Memory
updateMemory memory w8 address =
  if address < 128 -- RAM lives in $80-$FF.
    then
      Left
        "Tried to write to a RAM byte below $80. Perhaps you meant to write to a TIA register?"
    else
      Right $ memory // [(finite . fromIntegral $ address - 128, w8)] -- Minus 128 to get it in the sized range.

{- | I know some carts can bank-switch into having more ROM, but
the 6507 can only see 4K at a time. (or is it 8K?)
-}
data Memory4K = Memory4K
  { memory4k :: Sized.Vector 4096 Word8
  , focus :: Integer
  {- ^ Should be a Word12, but no one cares about Word12s. Set this to -1 initially.
  The focus should always be on the next word8 to be edited, not the word that was just edited.
  -}
  }

-- | You don't "burn" memory onto an Atari, especially not to an EPROM chip.
updateRom :: Memory4K -> Word8 -> Either T.Text Memory4K
updateRom rom4k w8 =
  if rom4k.focus + 1 >= 4096 -- 0-indexed, remember?
    then
      Left "ROM Overflow."
    else
      Right $
        Memory4K
          (rom4k.memory4k // [(finite rom4k.focus, w8)])
          (rom4k.focus + 1) -- Hey, this can overflow! Maybe add some error-checking?

{- | The Atari and the TV are so intertwined that you can't really have one
without the other.

Technically, counting things on the TV is a mixed radix system.

Frame - Line - HPos have bases Inf, 262, 76.

How many cycles have passed is just
  (Frame * 19912) + (Line * 76) + HPos 

-}
data TV = TV
  { frame :: Int
  , section :: Section
  , line :: Int
  -- ^ vertical position, 0-261 for NTSC.
  -- 
  , hPos :: Int
  {- ^ horizontal position, 68 "pixels" of hblank, 160 pixels of color
  one cpu cycle is 3 "pixels", so we get 76 cycles per line. This is 0-indexed.
  -}
  , region :: Region
  }
  deriving (Generic.Generic)

advanceTV :: Int -> TV -> TV
advanceTV cyclesToAdvance television =
  let
    current = (television.frame * 19912) + (television.line * 76) + television.hPos
    advanced = current + cyclesToAdvance
    (advFrame, remFrame) = advanced `quotRem` 19912 -- 262 * 76
    (advLine, advHPos) = remFrame `quotRem` 76 -- 76 :P
  in
    TV
      { frame = advFrame
      , section = television.section -- TODO: Actually update the section.
      , line = advLine
      , hPos = advHPos
      , region = television.region -- Imagine changing the region of a console while it's on.
      }

data PIA = PIA
  { swcha :: Word8
  -- ^ \$280, Port A; input or output (read or write)
  , swacnt :: Word8
  -- ^ \$281, Port A DDR, 0= input, 1=output
  , swchb :: Word8
  -- ^ \$282, Port B; console switches (read only)
  , swbcnt :: Word8
  -- ^ \$283, Port B DDR (hardwired as input)
  , intim :: Word8
  -- ^ \$284, Timer output (read only)
  , tim1t :: Word8
  -- ^ \$294, set 1 clock interval (838 nsec/interval)
  , tim8t :: Word8
  -- ^ \$295, set 8 clock interval (6.7 usec/interval)
  , tim64t :: Word8
  -- ^ \$296, set 64 clock interval (53.6 usec/interval)
  , t1024t :: Word8
  -- ^ \$297, set 1024 clock interval (858.2 usec/interval)
  }
  deriving (Generic.Generic)

data TIA = TIA
  { vsync :: Word8
  -- ^ \$00 - ......1. - vertical sync set-clear
  , vblank :: Word8
  -- ^ \$01 - 11....1. - vertical blank set-clear
  , wsync :: Word8
  -- ^ \$02 - strobe - wait for leading edge of horizontal blank
  , rsync :: Word8
  -- ^ \$03 - strobe - reset horizontal sync counter
  , nusiz0 :: Word8
  -- ^ number-size player-missile 0
  , nusiz1 :: Word8
  -- ^ number-size player-missile 1
  , colup0 :: Word8
  -- ^ color-lum player 0
  , colup1 :: Word8
  -- ^ color-lum player 1
  , colupf :: Word8
  -- ^ color-lum playfield
  , colubk :: Word8
  -- ^ color-lum background
  , ctrlpf :: Word8
  -- ^ control playfield ball size & collisions
  , refp0 :: Word8
  -- ^ reflect player 0
  , refp1 :: Word8
  -- ^ reflect player 1
  , pf0 :: Word8
  -- ^ playfield register byte 0
  , pf1 :: Word8
  -- ^ playfield register byte 1
  , pf2 :: Word8
  -- ^ playfield register byte 2
  , resp0 :: Word8
  -- ^ reset player 0
  , resp1 :: Word8
  -- ^ reset player 1
  , resm0 :: Word8
  -- ^ reset missile 0
  , resm1 :: Word8
  -- ^ reset missile 1
  , resbl :: Word8
  -- ^ reset ball
  , audc0 :: Word8
  -- ^ audio control 0
  , audc1 :: Word8
  -- ^ audio control 1
  , audf0 :: Word8
  -- ^ audio frequency 0
  , audf1 :: Word8
  -- ^ audio frequency 1
  , audv0 :: Word8
  -- ^ audio volume 0
  , audv1 :: Word8
  -- ^ audio volume 1
  , grp0 :: Word8
  -- ^ graphics player 0
  , grp1 :: Word8
  -- ^ graphics player 1
  , enam0 :: Word8
  -- ^ graphics (enable) missile 0
  , enam1 :: Word8
  -- ^ graphics (enable) missile 1
  , enabl :: Word8
  -- ^ graphics (enable) ball
  , hmp0 :: Word8
  -- ^ horizontal motion player 0
  , hmp1 :: Word8
  -- ^ horizontal motion player 1
  , hmm0 :: Word8
  -- ^ horizontal motion missile 0
  , hmm1 :: Word8
  -- ^ horizontal motion missile 1
  , hmbl :: Word8
  -- ^ horizontal motion ball
  , vdelp0 :: Word8
  -- ^ vertical delay player 0
  , vdelp1 :: Word8
  -- ^ vertical delay player 1
  , vdelbl :: Word8
  -- ^ vertical delay ball
  , resmp0 :: Word8
  -- ^ reset missile 0 to player 0
  , resmp1 :: Word8
  -- ^ reset missile 1 to player 1
  , hmove :: Word8
  -- ^ apply horizontal motion
  , hmclr :: Word8
  -- ^ clear horizontal motion registers
  , csclr :: Word8
  -- ^ clear collision latches
  }
  deriving (Generic.Generic)

data Region = NTSC | PAL | SECAM -- we're only supporting NTSC for rn.

-- | Abstract representation of where on the TV screen the beam is.
data Section = VSync | VBlank | HBlank | Color | Overscan

data MOS6507 = MOS6507
  { generalRegisters :: Registers
  , programCounter :: Word16
  , stackPointer :: Word8
  , statusRegister :: Flags
  , cpuCycle :: Int
  }
  deriving (Generic.Generic)

data Registers = Registers
  { a :: Word8
  , x :: Word8
  , y :: Word8
  }
  deriving (Generic.Generic)

data Env = Env
  { assembler :: Switch
  , emulator :: Switch
  , display :: Switch
  }

data Switch = On | Off

data Instruction = Instruction
  { shorthand :: Shorthand
  , addressingMode :: AddressingMode
  , opcode :: Word8
  , bytes :: Int
  , cycles :: Int
  , flags :: Flags
  , description :: T.Text
  }

data AddressingMode
  = Accumulator
  | Implied
  | Immediate
  | IndirectX
  | IndirectY
  | Relative
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | Indirect
  deriving (Eq)

data Flags
  = Flags
  { negative :: Bool
  , overflow :: Bool
  , ignored :: Bool
  , break :: Bool
  , decimal :: Bool
  , interrupt :: Bool
  , zero :: Bool
  , carry :: Bool
  }

emptyFlags :: Flags
emptyFlags = Flags False False False False False False False False

data Shorthand
  = ADC
  | ANC
  | AND
  | ANE
  | ALR
  | ARR
  | ASL
  | BCC
  | BCS
  | BEQ
  | BIT
  | BMI
  | BNE
  | BPL
  | BRK
  | BVC
  | BVS
  | CLC
  | CLD
  | CLI
  | CLV
  | CMP
  | CPX
  | CPY
  | DCP
  | DEC
  | DEX
  | DEY
  | EOR
  | INC
  | INX
  | INY
  | ISC
  | JAM
  | JMP
  | JSR
  | LAS
  | LAX
  | LDA
  | LDX
  | LDY
  | LSR
  | LXA
  | NOP
  | ORA
  | PHA
  | PHP
  | PLA
  | PLP
  | RLA
  | ROL
  | ROR
  | RRA
  | RTI
  | RTS
  | SAX
  | SBC
  | SBX
  | SEC
  | SED
  | SEI
  | SHA
  | SHY
  | SLO
  | SRE
  | STA
  | STX
  | STY
  | TAS
  | TAX
  | TAY
  | TSX
  | TXA
  | TXS
  | TYA
  | UBC
  deriving (Eq, Ord, Show)

type ValidInstructions = InsOrd.InsOrdHashMap Word8 Instruction

validInstructions :: InsOrd.InsOrdHashMap Word8 Instruction
validInstructions = InsOrd.fromList $ zip ([0..255] :: [Word8])
  [ Instruction BRK Implied 0x00 1 7 emptyFlags "Force Break"
  , Instruction ORA IndirectX 0x01 2 6 emptyFlags "OR Memory with Accumulator"
  , Instruction JAM Implied 0x02 0 0 emptyFlags "Freeze the CPU"
  , Instruction SLO IndirectX 0x03 2 8 emptyFlags "ASL op + ORA op"
  , Instruction NOP ZeroPage 0x04 2 3 emptyFlags "No operation"
  , Instruction ORA ZeroPage 0x05 2 3 emptyFlags "OR Memory with Accumulator"
  , Instruction ASL ZeroPage 0x06 2 5 emptyFlags "Shift Left One Bit"
  , Instruction SLO ZeroPage 0x07 2 5 emptyFlags "ASL op + ORA op"
  , Instruction PHP Implied 0x08 1 3 emptyFlags "Push Processor Status on Stack"
  , Instruction ORA Immediate 0x09 2 2 emptyFlags "OR Memory with Accumulator"
  , Instruction ASL Accumulator 0x0A 1 2 emptyFlags "Shift Left One Bit"
  , Instruction ANC Immediate 0x0B 2 2 emptyFlags "AND oper + set C as ASL"
  , Instruction NOP Absolute 0x0C 3 4 emptyFlags "No operation"
  , Instruction ORA Absolute 0x0D 3 4 emptyFlags "OR Memory with Accumulator"
  , Instruction ASL Absolute 0x0E 3 6 emptyFlags "Shift Left One Bit"
  , Instruction SLO Absolute 0x0F 3 6 emptyFlags "ASL op + ORA op"
  , Instruction BPL Relative 0x10 2 2 emptyFlags "Branch on N = 0"
  , Instruction ORA IndirectY 0x11 2 5 emptyFlags "OR Memory with Accumulator"
  , Instruction JAM Implied 0x12 0 0 emptyFlags "Freeze the CPU"
  , Instruction SLO IndirectY 0x13 2 8 emptyFlags "ASL op + ORA op"
  , Instruction NOP ZeroPageX 0x14 2 4 emptyFlags "No operation"
  , Instruction ORA ZeroPageX 0x15 2 4 emptyFlags "OR Memory with Accumulator"
  , Instruction ASL ZeroPageX 0x16 2 6 emptyFlags "Shift Left One Bit"
  , Instruction SLO ZeroPageX 0x17 2 5 emptyFlags "ASL op + ORA op"
  , Instruction CLC Implied 0x18 1 2 emptyFlags "Clear Carry Flag"
  , Instruction ORA AbsoluteY 0x19 3 4 emptyFlags "OR Memory with Accumulator"
  , Instruction NOP Implied 0x1A 1 2 emptyFlags "No operation"
  , Instruction SLO AbsoluteY 0x1B 3 7 emptyFlags "ASL op + ORA op"
  , Instruction NOP AbsoluteX 0x1C 1 2 emptyFlags "No operation"
  , Instruction ORA AbsoluteX 0x1D 3 4 emptyFlags "OR Memory with Accumulator"
  , Instruction ASL AbsoluteX 0x1E 3 7 emptyFlags "Shift Left One Bit"
  , Instruction SLO AbsoluteX 0x1F 3 7 emptyFlags "ASL oper + ORA oper"
  , Instruction JSR Absolute 0x20 3 6 emptyFlags "Jump to subroutine"
  , Instruction AND IndirectX 0x21 2 6 emptyFlags "AND memory with accumulator"
  , Instruction JAM Implied 0x22 0 0 emptyFlags "Freeze the CPU"
  , Instruction RLA IndirectX 0x23 2 8 emptyFlags "ROL oper + AND oper"
  , Instruction BIT ZeroPage 0x24 2 3 emptyFlags "Test Bits in Memory with accumulator"
  , Instruction AND ZeroPage 0x25 2 3 emptyFlags "AND Memory with Accumulator"
  , Instruction ROL ZeroPage 0x26 2 5 emptyFlags "Rotate One Bit Left"
  , Instruction RLA ZeroPage 0x27 2 5 emptyFlags "ROL oper + AND oper"
  , Instruction PLP Implied 0x28 1 4 emptyFlags "Pull processor status from stack"
  , Instruction AND Immediate 0x29 2 2 emptyFlags "AND Memory with Accumulator"
  , Instruction ROL Accumulator 0x2A 1 2 emptyFlags "Rotate One Bit Left"
  , Instruction ANC Immediate 0x2B 2 2 emptyFlags "AND oper + set C as ASL"
  , Instruction BIT Absolute 0x2C 3 4 emptyFlags "Test Bits in Memory with accumulator"
  , Instruction AND Absolute 0x2D 3 4 emptyFlags "AND memory with accumulator"
  , Instruction ROL Absolute 0x2E 3 6 emptyFlags "Rotate One Bit Left"
  , Instruction RLA Absolute 0x2F 3 6 emptyFlags "ROL oper + AND oper"
  , Instruction BMI Relative 0x30 2 2 emptyFlags "Branch on Result Minus"
  , Instruction AND IndirectY 0x31 2 5 emptyFlags "AND Memory with Accumulator"
  , Instruction JAM Implied 0x32 0 0 emptyFlags "Freeze the CPU"
  , Instruction RLA IndirectY 0x33 2 8 emptyFlags "ROL Oper + AND oper"
  , Instruction NOP ZeroPageX 0x34 2 4 emptyFlags "No operation"
  , Instruction AND ZeroPageX 0x35 2 4 emptyFlags "AND Memory with Accumulator"
  , Instruction ROL ZeroPageX 0x36 2 6 emptyFlags "Rotate One Bit Left"
  , Instruction RLA ZeroPageX 0x37 2 6 emptyFlags "ROL Oper + AND oper"
  , Instruction SEC Implied 0x38 1 2 emptyFlags "Set Carry Flag"
  , Instruction AND AbsoluteY 0x39 3 4 emptyFlags "AND Memory with Accumulator"
  , Instruction NOP Implied 0x3A 1 2 emptyFlags "No operation"
  , Instruction RLA AbsoluteY 0x3B 3 7 emptyFlags "ROL Oper + AND oper"
  , Instruction NOP AbsoluteX 0x3C 3 4 emptyFlags "No operation"
  , Instruction AND AbsoluteX 0x3D 3 4 emptyFlags "AND Memory with Accumulator"
  , Instruction ROL AbsoluteX 0x3E 3 7 emptyFlags "Rotate One Bit Left"
  , Instruction RLA AbsoluteX 0x3F 3 7 emptyFlags "ROL Oper + AND oper"
  , Instruction RTI Implied 0x40 1 6 emptyFlags "Return from Interrupt"
  , Instruction EOR IndirectX 0x41 2 6 emptyFlags "Exclusive-OR Memory with Accumulator"
  , Instruction JAM Implied 0x42 0 0 emptyFlags "Freeze the CPU"
  , Instruction SRE IndirectX 0x43 2 8 emptyFlags "LSR oper + EOR oper"
  , Instruction NOP ZeroPage 0x44 2 3 emptyFlags "No operation"
  , Instruction EOR ZeroPage 0x45 2 3 emptyFlags "Exclusive-OR Memory with Accumulator"
  , Instruction LSR ZeroPage 0x46 2 5 emptyFlags "Shift One Bit Right"
  , Instruction SRE ZeroPage 0x47 2 5 emptyFlags "LSR oper + EOR oper"
  , Instruction PHA Implied 0x48 1 3 emptyFlags "Push Accumulator on Stack"
  , Instruction EOR Immediate 0x49 2 2 emptyFlags "Exclusive-OR Memory with Accumulator"
  , Instruction LSR Accumulator 0x4A 1 2 emptyFlags "Shift One Bit Right"
  , Instruction ALR Immediate 0x4B 2 2 emptyFlags "AND oper + LSR"
  , Instruction JMP Absolute 0x4C 3 3 emptyFlags "Jump to New Location"
  , Instruction EOR Absolute 0x4D 3 4 emptyFlags "Exclusive-OR Memory with Accumulator"
  , Instruction LSR Absolute 0x4E 3 6 emptyFlags "Shift One Bit Right"
  , Instruction SRE Absolute 0x4F 3 6 emptyFlags "LSR oper + EOR oper"
  , Instruction BVC Relative 0x50 2 2 emptyFlags "Branch on Overflow Clear"
  , Instruction EOR IndirectY 0x51 2 5 emptyFlags "Exclusive-OR Memory with Accumulator"
  , Instruction JAM Implied 0x52 0 0 emptyFlags "Freeze the CPU"
  , Instruction SRE IndirectY 0x53 2 8 emptyFlags "LSR oper + EOR oper"
  , Instruction NOP ZeroPageX 0x54 2 4 emptyFlags "No operation"
  , Instruction EOR ZeroPageX 0x55 2 4 emptyFlags "Exclusive-OR Memory with Accumulator"
  , Instruction LSR ZeroPageX 0x56 2 6 emptyFlags "Shift One Bit Right"
  , Instruction SRE ZeroPageX 0x57 2 6 emptyFlags "LSR oper + EOR oper"
  , Instruction CLI Implied 0x58 1 2 emptyFlags "Clear Interrupr Disable Bit"
  , Instruction EOR AbsoluteY 0x59 3 4 emptyFlags "Exclusive-OR Memory with Accumulator"
  , Instruction NOP Implied 0x5A 1 2 emptyFlags "No operation"
  , Instruction SRE AbsoluteY 0x5B 3 7 emptyFlags "LSR oper + EOR oper"
  , Instruction NOP AbsoluteX 0x5C 3 4 emptyFlags "No operation"
  , Instruction EOR AbsoluteX 0x5D 3 4 emptyFlags "Exclusive-OR Memory with Accumulator"
  , Instruction LSR AbsoluteX 0x5E 3 7 emptyFlags "Shift One Bit Right"
  , Instruction SRE AbsoluteX 0x5F 3 7 emptyFlags "LSR oper + EOR oper"
  , Instruction RTS Implied 0x60 1 6 emptyFlags "Return from Subroutine"
  , Instruction ADC IndirectX 0x61 2 6 emptyFlags "Add Memory to Accumulator with Carry"
  , Instruction JAM Implied 0x62 0 0 emptyFlags "Freeze the CPU"
  , Instruction RRA IndirectX 0x63 2 8 emptyFlags "ROR oper + ADC oper"
  , Instruction NOP ZeroPage 0x64 2 3 emptyFlags "No operation"
  , Instruction ADC ZeroPage 0x65 2 3 emptyFlags "Add Memory to Accumulator with Carry"
  , Instruction ROR ZeroPage 0x66 2 5 emptyFlags "Rotate One Bit Right"
  , Instruction RRA ZeroPage 0x67 2 5 emptyFlags "ROR oper + ADC oper"
  , Instruction PLA Implied 0x68 1 4 emptyFlags "Pull Accumulator from Stack"
  , Instruction ADC Immediate 0x69 2 2 emptyFlags "Add Memory to Accumulator with Carry"
  , Instruction ROR Accumulator 0x6A 1 2 emptyFlags "Rotate One Bit Right"
  , Instruction ARR Immediate 0x6B 2 2 emptyFlags "AND oper + ROR"
  , Instruction JMP Indirect 0x6C 3 5 emptyFlags "Jump to New Location"
  , Instruction ADC Absolute 0x6D 3 4 emptyFlags "Add Memory to Accumulator with Carry"
  , Instruction ROR Absolute 0x6E 3 6 emptyFlags "Rotate One Bit Right"
  , Instruction RRA Absolute 0x6F 3 6 emptyFlags "ROR oper + ADC oper"
  , Instruction BVS Relative 0x70 2 2 emptyFlags "Branch on Overflow Set"
  , Instruction ADC IndirectY 0x71 2 5 emptyFlags "Add Memory to Accumulator with Carry"
  , Instruction JAM Implied 0x72 0 0 emptyFlags "Freeze the CPU"
  , Instruction RRA IndirectY 0x73 2 8 emptyFlags "ROR oper + ADC oper"
  , Instruction NOP ZeroPageX 0x74 2 4 emptyFlags "No operation"
  , Instruction ADC ZeroPageX 0x75 2 4 emptyFlags "Add Memory to Accumulator with Carry"
  , Instruction ROR ZeroPageX 0x76 2 6 emptyFlags "Rotate One Bit Right"
  , Instruction RRA ZeroPageX 0x77 2 6 emptyFlags "ROR oper + ADC oper"
  , Instruction SEI Implied 0x78 1 2 emptyFlags "Set Interrupte Disable Status"
  , Instruction ADC AbsoluteY 0x79 3 4 emptyFlags "Add Memory to Accumulator with Carry"
  , Instruction NOP Implied 0x7A 1 2 emptyFlags "No operation"
  , Instruction RRA AbsoluteY 0x7B 3 7 emptyFlags "ROR oper + ADC oper"
  , Instruction NOP AbsoluteX 0x7C 3 4 emptyFlags "No operation"
  , Instruction ADC AbsoluteX 0x7D 3 4 emptyFlags "Add Memory to Accumulator with Carry"
  , Instruction ROR AbsoluteX 0x7E 3 7 emptyFlags "Rotate One Bit Right"
  , Instruction RRA AbsoluteX 0x7F 3 7 emptyFlags "ROR oper + ADC oper"
  , Instruction NOP Immediate 0x80 2 2 emptyFlags "No operation"
  , Instruction STA IndirectX 0x81 2 6 emptyFlags "Store Accumulator in Memory"
  , Instruction NOP Immediate 0x82 2 2 emptyFlags "No operation"
  , Instruction SAX IndirectX 0x83 2 6 emptyFlags "A AND X -> Memory"
  , Instruction STY ZeroPage 0x84 2 3 emptyFlags "Store Y in Memory"
  , Instruction STA ZeroPage 0x85 2 3 emptyFlags "Store Accumulator in Memory"
  , Instruction STX ZeroPage 0x86 2 3 emptyFlags "Store X In Memory"
  , Instruction SAX ZeroPage 0x87 2 3 emptyFlags "A AND X -> Memory"
  , Instruction DEY Implied 0x88 1 2 emptyFlags "Decrement Y"
  , Instruction NOP Immediate 0x89 2 2 emptyFlags "No operation"
  , Instruction TXA Implied 0x8A 1 2 emptyFlags "Transfer X to Accumulator"
  , Instruction ANE Immediate 0x8B 2 2 emptyFlags "OR X + AND oper, HIGHLY UNSTABLE"
  , Instruction STY Absolute 0x8C 3 4 emptyFlags "Store Y in Memory"
  , Instruction STA Absolute 0x8D 3 4 emptyFlags "Store Accumulator in Memory"
  , Instruction STX Absolute 0x8E 3 4 emptyFlags "Store X In Memory"
  , Instruction SAX Absolute 0x8F 3 4 emptyFlags "A AND X -> Memory"
  , Instruction BCC Relative 0x90 2 2 emptyFlags "Branch on Carry Clear"
  , Instruction STA IndirectY 0x91 2 6 emptyFlags "Store Accumulator in Memory"
  , Instruction JAM Implied 0x92 0 0 emptyFlags "Freeze the CPU"
  , Instruction SHA IndirectY 0x93 2 6 emptyFlags "A AND X AND (H+1) -> M, UNSTABLE"
  , Instruction STY ZeroPageX 0x94 2 4 emptyFlags "Store Y in Memory"
  , Instruction STA ZeroPageX 0x95 2 4 emptyFlags "Store Accumulator in Memory"
  , Instruction STX ZeroPageY 0x96 2 4 emptyFlags "Store X In Memory"
  , Instruction SAX ZeroPageY 0x97 2 4 emptyFlags "A AND X -> Memory"
  , Instruction TYA Implied 0x98 1 2 emptyFlags "Transfer Y to Accumulator"
  , Instruction STA AbsoluteY 0x99 3 5 emptyFlags "Store Accumulator in Memory"
  , Instruction TXS Implied 0x9A 1 2 emptyFlags "Transfer X to Stack Register"
  , Instruction TAS AbsoluteY 0x9B 3 5 emptyFlags "A AND X -> SP, A AND X AND (H+1) -> M, UNSTABLE"
  , Instruction SHY AbsoluteX 0x9C 3 5 emptyFlags "Y AND (H+1) -> M, UNSTABLE"
  , Instruction STA AbsoluteX 0x9D 3 5 emptyFlags "Store Accumulator in Memory"
  , Instruction SHA AbsoluteY 0x9F 3 5 emptyFlags "A AND X AND (H+1) -> M, UNSTABLE"
  , Instruction LDY Immediate 0xA0 2 2 emptyFlags "Load Y with Memory"
  , Instruction LDA IndirectX 0xA1 2 6 emptyFlags "Load Accumulator with Memory"
  , Instruction LDX Immediate 0xA2 2 2 emptyFlags "Load X with Memory"
  , Instruction LAX IndirectX 0xA3 2 6 emptyFlags "LDA oper + LDX oper"
  , Instruction LDY ZeroPage 0xA4 2 3 emptyFlags "Load Y into Memory"
  , Instruction LDA ZeroPage 0xA5 2 3 emptyFlags "Load Accumulator with Memory"
  , Instruction LDX ZeroPage 0xA6 2 3 emptyFlags "Load X with Memory"
  , Instruction LAX ZeroPage 0xA7 2 3 emptyFlags "LDA oper + LDX oper"
  , Instruction TAY Implied 0xA8 1 2 emptyFlags "Transfer Accumulator to Y"
  , Instruction LDA Immediate 0xA9 2 2 emptyFlags "Load Accumulator with Memory"
  , Instruction TAX Implied 0xAA 1 2 emptyFlags "Transfer Accumulator to A"
  , Instruction LXA Immediate 0xAB 2 2 emptyFlags "Store * AND oper in A and X, HIGHLY UNSTABLE"
  , Instruction LDY Absolute 0xAC 3 4 emptyFlags "Load Y into Memory"
  , Instruction LDA Absolute 0xAD 3 4 emptyFlags "Load Accumulator with Memory"
  , Instruction LDX Absolute 0xAE 3 4 emptyFlags "Load X with Memory"
  , Instruction LAX Absolute 0xAF 3 4 emptyFlags "LDA oper + LDX oper"
  , Instruction BCS Relative 0xB0 2 2 emptyFlags "Branch on Carry Set"
  , Instruction LDA IndirectY 0xB1 2 5 emptyFlags "Load Accumulator with Memory"
  , Instruction JAM Implied 0xB2 0 0 emptyFlags "Freeze the CPU"
  , Instruction LAX IndirectY 0xB3 2 5 emptyFlags "LDA oper + LDX oper"
  , Instruction LDY ZeroPageX 0xB4 2 4 emptyFlags "Load Y into Memory"
  , Instruction LDA ZeroPageX 0xB5 2 4 emptyFlags "Load Accumulator with Memory"
  , Instruction LDX ZeroPageY 0xB6 2 4 emptyFlags "Load X with Memory"
  , Instruction LAX ZeroPageY 0xB7 2 4 emptyFlags "LDA oper + LDX oper"
  , Instruction CLV Implied 0xB8 1 2 emptyFlags "Clear Overflow Flag"
  , Instruction LDA AbsoluteY 0xB9 3 4 emptyFlags "Load Accumulator with Memory"
  , Instruction TSX Implied 0xBA 1 2 emptyFlags "Transfer Stack Pointer to X"
  , Instruction LAS AbsoluteY 0xBB 3 4 emptyFlags "LDA/TSX oper"
  , Instruction LDY AbsoluteX 0xBC 3 4 emptyFlags "Load Y into Memory"
  , Instruction LDA AbsoluteX 0xBD 3 4 emptyFlags "Load Accumulator with Memory"
  , Instruction LDX AbsoluteY 0xBE 3 4 emptyFlags "Load X with Memory"
  , Instruction LAX AbsoluteY 0xBF 3 4 emptyFlags "LDA oper + LDX oper"
  , Instruction CPY Immediate 0xC0 2 2 emptyFlags "Compare Memory and Y"
  , Instruction CMP IndirectX 0xC1 2 6 emptyFlags "Compare Memory with Accumulator"
  , Instruction NOP Immediate 0xC2 2 2 emptyFlags "No operation"
  , Instruction DCP IndirectX 0xC3 2 8 emptyFlags "DEC oper + CMP oper"
  , Instruction CPY ZeroPage 0xC4 2 3 emptyFlags "Compare Memory and Y"
  , Instruction CMP ZeroPage 0xC5 2 3 emptyFlags "Compare Memory with Accumulator"
  , Instruction DEC ZeroPage 0xC6 2 5 emptyFlags "Decrement Memory by One"
  , Instruction DCP ZeroPage 0xC7 2 5 emptyFlags "DEC oper + CMP oper"
  , Instruction INY Implied 0xC8 1 2 emptyFlags "Increment Y by One"
  , Instruction CMP Immediate 0xC9 2 2 emptyFlags "Compare Memory with Accumulator"
  , Instruction DEX Implied 0xCA 1 2 emptyFlags "Decrement X by One"
  , Instruction SBX Immediate 0xCB 2 2 emptyFlags "CMP oper + DEX oper"
  , Instruction CPY Absolute 0xCC 3 4 emptyFlags "Compare Memory and Y"
  , Instruction CMP Absolute 0xCD 3 4 emptyFlags "Compare Memory with Accumulator"
  , Instruction DEC Absolute 0xCE 3 6 emptyFlags "Decrement Memory by One"
  , Instruction DCP Absolute 0xCF 3 6 emptyFlags "DEC oper + CMP oper"
  , Instruction BNE Relative 0xD0 2 2 emptyFlags "Branch on Result not Zero"
  , Instruction CMP IndirectY 0xD1 2 5 emptyFlags "Compare Memory with Accumulator"
  , Instruction JAM Implied 0xD2 0 0 emptyFlags "Freeze the CPU"
  , Instruction DCP IndirectY 0xD3 2 8 emptyFlags "DEC oper + CMP oper"
  , Instruction NOP ZeroPageX 0xD4 2 4 emptyFlags "No operation"
  , Instruction CMP ZeroPageX 0xD5 2 4 emptyFlags "Compare Memory with Accumulator"
  , Instruction DEC ZeroPageX 0xD6 2 6 emptyFlags "Decrement Memory by One"
  , Instruction DCP ZeroPageX 0xD7 2 6 emptyFlags "DEC oper + CMP oper"
  , Instruction CLD Implied 0xD8 1 2 emptyFlags "Clear Decimal Mode"
  , Instruction CMP AbsoluteY 0xD9 3 4 emptyFlags "Compare Memory with Accumulator"
  , Instruction NOP Implied 0xDA 1 2 emptyFlags "No operation"
  , Instruction DCP AbsoluteY 0xDB 3 7 emptyFlags "DEC oper + CMP oper"
  , Instruction NOP AbsoluteX 0xDC 3 4 emptyFlags "No operation"
  , Instruction CMP AbsoluteX 0xDD 3 4 emptyFlags "Compare Memory with Accumulator"
  , Instruction DEC AbsoluteX 0xDE 3 7 emptyFlags "Decrement Memory by One"
  , Instruction DCP AbsoluteX 0xDF 3 7 emptyFlags "DEC oper + CMP oper"
  , Instruction CPX Immediate 0xE0 2 2 emptyFlags "Compare Memory and X"
  , Instruction SBC IndirectX 0xE1 2 6 emptyFlags "Subtract Memory from Accumulator with Borrow"
  , Instruction NOP Immediate 0xE2 2 2 emptyFlags "No operation"
  , Instruction ISC IndirectX 0xE3 2 8 emptyFlags "INC oper + CMP oper"
  , Instruction CPX ZeroPage 0xE4 2 3 emptyFlags "Compare Memory and X"
  , Instruction SBC ZeroPage 0xE5 2 3 emptyFlags "Subtract Memory from Accumulator with Borrow"
  , Instruction INC ZeroPage 0xE6 2 5 emptyFlags "Increment Memory by One"
  , Instruction ISC ZeroPage 0xE7 2 5 emptyFlags "INC oper + CMP oper"
  , Instruction INX Implied 0xE8 1 2 emptyFlags "Increment X by One"
  , Instruction SBC Immediate 0xE9 2 2 emptyFlags "Subtract Memory from Accumulator with Borrow"
  , Instruction NOP Immediate 0xEA 1 2 emptyFlags "No operation"
  , Instruction UBC Immediate 0xEB 2 2 emptyFlags "SBC oper + NOP"
  , Instruction CPX Absolute 0xEC 3 4 emptyFlags "Compare Memory and X"
  , Instruction SBC Absolute 0xED 3 4 emptyFlags "Subtract Memory from Accumulator with Borrow"
  , Instruction INC Absolute 0xEE 3 6 emptyFlags "Increment Memory by One"
  , Instruction ISC Absolute 0xEF 3 6 emptyFlags "INC oper + CMP oper"
  , Instruction BEQ Relative 0xF0 2 2 emptyFlags "Branch on Result Zero"
  , Instruction SBC IndirectY 0xF1 2 5 emptyFlags "Subtract Memory from Accumulator with Borrow"
  , Instruction JAM Implied 0xF2 0 0 emptyFlags "Freeze the CPU"
  , Instruction ISC IndirectY 0xF3 2 8 emptyFlags "INC oper + CMP oper"
  , Instruction NOP ZeroPageX 0xF4 2 4 emptyFlags "No operation"
  , Instruction SBC ZeroPageX 0xF5 2 4 emptyFlags "Subtract Memory from Accumulator with Borrow"
  , Instruction INC ZeroPageX 0xF6 2 6 emptyFlags "Increment Memory by One"
  , Instruction ISC ZeroPageX 0xF7 2 6 emptyFlags "INC oper + CMP oper"
  , Instruction SED Implied 0xF8 1 2 emptyFlags "Set Decimal Flag"
  , Instruction SBC AbsoluteY 0xF9 3 4 emptyFlags "Subtract Memory from Accumulator with Borrow"
  , Instruction NOP Implied 0xFA 1 2 emptyFlags "No operation"
  , Instruction ISC AbsoluteY 0xFB 3 7 emptyFlags "INC oper + CMP oper"
  , Instruction NOP AbsoluteX 0xFC 3 4 emptyFlags "No operation"
  , Instruction SBC AbsoluteX 0xFD 3 4 emptyFlags "Subtract Memory from Accumulator with Borrow"
  , Instruction INC AbsoluteX 0xFE 3 7 emptyFlags "Increment Memory by One"
  , Instruction ISC AbsoluteX 0xFF 3 7 emptyFlags "INC oper + CMP oper"
  ]

jamInstruction :: Instruction
jamInstruction = Instruction JAM Implied 0x02 0 0 emptyFlags "Freeze the CPU"
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
