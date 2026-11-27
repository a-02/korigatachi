{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Korigatachi.Types
  ( AddressingMode (..)
  , ASMArg (..)
  , ASMBinOp
  , ASMTag (..)
  , ASMOp
  , Assembly
  , BaseRepresentation (..)
  , Bin (..)
  , Env (..)
  , Gukei (..)
  , Hane
  , Instruction (..)
  , IntLike
  , Katteyomi (..)
  , Length (..)
  , LogLevel (..)
  , Operand (..)
  , Resolver (..)
  , Shorthand (..)
  , Statement (..)
  , Statements (..)
  , TextLike
  , module Resolve.Types.Export
  )
where

import Data.Word (Word16, Word32, Word8)
import Language.Haskell.TH.Syntax (Lift)
import Prelude hiding (break)

import Data.ByteString qualified as BS
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import GHC.Generics qualified as Generic

import Korigatachi.Monad
import Korigatachi.Resolve.Types as Resolve.Types.Export

-- | An `Operand` represents an argument along with its addressing mode..
data Operand
  = Accumulator
  | Implied
  | Immediate Word8
  | IndirectX Word8
  | IndirectY Word8
  | Relative Word8
  | ZeroPage Word8
  | ZeroPageX Word8
  | ZeroPageY Word8
  | Absolute Word8 Word8
  | AbsoluteX Word8 Word8
  | AbsoluteY Word8 Word8
  | Indirect Word8 Word8
  | Unresolved T.Text
  deriving (Eq, Ord, Show)

data AddressingMode
  = AMAccumulator
  | AMImplied
  | AMImmediate
  | AMIndirectX
  | AMIndirectY
  | AMRelative
  | AMZeroPage
  | AMZeroPageX
  | AMZeroPageY
  | AMAbsolute
  | AMAbsoluteX
  | AMAbsoluteY
  | AMIndirect
  | AMUnresolved
  deriving (Eq)

instance Ord AddressingMode where
  (<=) am1 am2 =
    let addressingModePrecedence :: AddressingMode -> Int
        addressingModePrecedence = \case
          AMIndirectX -> 1
          AMIndirectY -> 2
          AMZeroPageX -> 3
          AMZeroPageY -> 4
          AMRelative -> 5
          AMZeroPage -> 6
          AMAbsolute -> 7
          AMAbsoluteX -> 8
          AMAbsoluteY -> 9
          AMIndirect -> 10
          AMImmediate -> 11
          AMImplied -> 12
          AMAccumulator -> 13
          _ -> maxBound
     in (addressingModePrecedence am1) <= (addressingModePrecedence am2)

instance Show AddressingMode where
  show = \case
    AMIndirectX -> "IndirectX"
    AMIndirectY -> "IndirectY"
    AMZeroPageX -> "ZeroPageX"
    AMZeroPageY -> "ZeroPageY"
    AMRelative -> "Relative"
    AMZeroPage -> "ZeroPage"
    AMAbsolute -> "Absolute"
    AMAbsoluteX -> "AbsoluteX"
    AMAbsoluteY -> "AbsoluteY"
    AMIndirect -> "Indirect"
    AMImmediate -> "Immediate"
    AMImplied -> "Implied"
    AMAccumulator -> "Accumulator"

-- | A statement is a single line of assembly.
data Statement
  = Local Word T.Text
  | Org Word16
  | Long Word32
  | Word Word16
  | Byte Word8
  | Pointer Length LabelAddressing T.Text
  | Processor T.Text
  | Include T.Text
  | TopLevelLabel T.Text
  | Instruct Shorthand Operand
  | Malformed T.Text
  | Val T.Text Word
  deriving (Show)

-- | There is death in the Hane.
type Hane start end return = RWIT Env Katteyomi IO start end return

{- | The first pass is called Assembly. It is meant to be used by the end user
to build assembly programs. Every other pass is something the user does not need to worry
about. The first pass translates the DSL into our internal Seq.Seq Statement.
-}
type Assembly a = Hane Statements Statements a

-- | From the Japanese for "selfish reading". The data structure for the writer.
data Katteyomi = Katteyomi
  { logs :: T.Text
  , codegen :: T.Text
  }
  deriving (Generic.Generic)

instance Semigroup Katteyomi where
  (<>) :: Katteyomi -> Katteyomi -> Katteyomi
  k1 <> k2 = Katteyomi (logs k1 <> logs k2) (codegen k1 <> codegen k2)

instance Monoid Katteyomi where
  mempty :: Katteyomi
  mempty = Katteyomi "" ""

-- | The list of Statement is constructed in order, so the O(1) append helps a ton.
data Statements = Statements {statements :: Seq.Seq Statement}
  deriving (Show)

data Resolver = Resolver
  { resolverStatements :: Seq.Seq Statement
  , resolverLabels :: Seq.Seq (Word16, T.Text)
  , resolverCodegen :: Seq.Seq T.Text
  , resolverProgramCounter :: Word16
  }
  deriving (Show)

data Bin = Bin
  {binOutput :: BS.ByteString}
  deriving (Show)

data Env = Env
  { logLevel :: LogLevel
  }

data LogLevel = Test | Info | Note | Warn | Crit
  deriving (Eq, Ord)

instance Show LogLevel where
  show Test = "[TEST] "
  show Info = "[INFO] "
  show Note = "[NOTE] "
  show Warn = "[WARN] "
  show Crit = "[CRIT] "

data BaseRepresentation = Binary | Octal | Decimal | Hexadecimal
  deriving (Eq, Ord)

data Length = DCByte | DCWord | DCLong deriving (Show)

data IntLike
data TextLike

data ASMTag tag where
  IntTag :: ASMTag IntLike
  TextTag :: ASMTag TextLike

-- | From the Japanese for "bad shape".
class Gukei a where
  gukei :: ASMTag a

instance Gukei IntLike where
  gukei :: ASMTag IntLike
  gukei = IntTag

instance Gukei TextLike where
  gukei :: ASMTag TextLike
  gukei = TextTag

type family ASMBinOp a b where
  ASMBinOp IntLike IntLike = IntLike
  ASMBinOp IntLike TextLike = TextLike
  ASMBinOp TextLike IntLike = TextLike
  ASMBinOp TextLike TextLike = TextLike

data ASMArg phantom where
  AAByte :: Word8 -> ASMArg IntLike
  AAWord :: Word16 -> ASMArg IntLike
  AALong :: Word32 -> ASMArg IntLike
  AAInt :: Integer -> ASMArg IntLike
  AAExpr :: T.Text -> ASMArg TextLike

deriving instance Eq (ASMArg a)

instance Semigroup (ASMArg TextLike) where
  (<>) :: ASMArg TextLike -> ASMArg TextLike -> ASMArg TextLike
  (AAExpr e1) <> (AAExpr e2) = AAExpr (e1 <> e2)

type ASMOp = forall a b. (Gukei a, Gukei b) => ASMArg a -> ASMArg b -> ASMArg (ASMBinOp a b)

data Instruction = Instruction
  { shorthand :: Shorthand
  , addressingMode :: AddressingMode
  , opcode :: Word8
  , bytes :: Int
  , cycles :: Int
  , description :: T.Text
  }
  deriving (Show)

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
  deriving (Eq, Ord, Show, Enum, Bounded, Lift)
