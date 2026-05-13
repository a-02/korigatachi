{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Korigatachi.Types where

import Data.Text qualified as T
import Data.Vector.Sized qualified as Sized
import Data.Word (Word16, Word8)
import GHC.Generics qualified as Generic
import Korigatachi.Monad
import Prelude hiding (break)

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
  | Label T.Text -- What did you do?
  deriving (Show)

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

data ProgramState
  = Running Atari
  | Jammed Atari
  | Error KorigatachiError
  | Stopped

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
  -- ^ Off-the-shelf 6532 Peripheral Interface Adapter.
  --   Technically, the RAM is controlled by this chip, but
  --   it's easier to abstract the RAM to a seperate field.
  }
  deriving (Generic.Generic)

{- | The Atari 2600 only had 128 bytes of RAM.
Frankly, I'm stunned it even has that much.
We can't even use some of these, maybe some type level
synonym is in order that has a list of fields we can't use.
-}
type Memory = Sized.Vector 128 Word8

data Env = Env
  { assembler :: Switch
  , emulator :: Switch
  , display :: Switch
  }

data Switch = On | Off

data Instruction = Instruction
  { shorthand :: Shorthand
  , addressingMode :: T.Text
  , opcode :: Word8
  , bytes :: Int
  , cycles :: Int
  , description :: T.Text
  }

data Flags = Flags
  { negative :: Bool
  , overflow :: Bool
  , ignored :: Bool
  , break :: Bool
  , decimal :: Bool
  , interrupt :: Bool
  , zero :: Bool
  , carry :: Bool
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

{- | I'm not gonna write out the addresses for all of these here.
Reference the pattern synonyms in Korigatachi.Assembly please.
Or find a copy of the Stella Programming Guide instead.
-}
data TIA = TIA
  { write :: WriteTIA
  , read :: ReadTIA
  }
  deriving (Generic.Generic)

-- No helpful comments for you right now.
data ReadTIA = ReadTIA
  { cxm0p :: Word8
  , cxm1p :: Word8
  , cxp0fb :: Word8
  , cxp1fb :: Word8
  , cxm0fb :: Word8
  , cxm1fb :: Word8
  , cxblpf :: Word8
  , cxppmm :: Word8
  , inpt0 :: Word8
  , inpt1 :: Word8
  , inpt2 :: Word8
  , inpt3 :: Word8
  , inpt4 :: Word8
  , inpt5 :: Word8
  }
  deriving (Generic.Generic)

-- | TIA Write Registers.
data WriteTIA = WriteTIA
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
  , cxclr :: Word8
  -- ^ clear collision latches
  }
  deriving (Generic.Generic)

data Registers = Registers
  { a :: Word8
  , x :: Word8
  , y :: Word8
  }
  deriving (Generic.Generic)

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
  | CLD -- Done.
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
  | LDA -- Done.
  | LDX -- Done.
  | LDY -- Done.
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
  | SEI -- Done.
  | SHA
  | SHY
  | SLO
  | SRE
  | STA -- Done.
  | STX
  | STY
  | TAS
  | TAX
  | TAY
  | TSX
  | TXA
  | TXS -- Done?
  | TYA
  | UBC
  deriving (Eq, Ord, Show)

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
  , hPos :: Int
  -- ^ horizontal position, 68 "pixels" of hblank, 160 pixels of color
  --   one cpu cycle is 3 "pixels", so we get 76 cycles per line. This is 0-indexed.
  , region :: Region
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

data Memory4K = Memory4K
  { memory4k :: Sized.Vector 4096 Word8
  , focus :: Word16
  , labels :: [MemoryLabel]
  }
  deriving (Generic.Generic)

data MemoryLabel = MemoryLabel T.Text Word16

data BaseRepresentation = Binary | Octal | Decimal | Hexadecimal
  deriving (Eq, Ord)
