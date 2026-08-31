{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Korigatachi.Types
  ( Operand (..)
  , Statement (..)
  , Hane
  , Assembly
  , Katteyomi (..)
  , Assemble (..)
  , Resolve (..)
  , Env (..)
  , LogLevel (..)
  , BaseRepresentation (..)
  , module Atari.Types.Export
  , module Resolve.Types.Export
  )
where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import GHC.Generics qualified as Generic
import Korigatachi.Atari.Types as Atari.Types.Export
import Korigatachi.Monad
import Korigatachi.Resolve.Types as Resolve.Types.Export
import Prelude hiding (break)

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
  | Label [LabelAddressing] T.Text
  deriving (Eq, Ord, Show)

data Statement
  = Org Word16
  | Word Word16
  | Processor T.Text
  | Include T.Text
  | TopLevelLabel T.Text
  | Instruct Shorthand Operand

-- -- | From the Japanese for "congealed shape". The monad to rule them all.
-- type Korigatachi a = RWIT Env Katteyomi IO Atari Atari a

-- | There is death in the Hane.
type Hane start end return = RWIT Env Katteyomi IO start end return

{- | The first pass is called Assembly. It is meant to be used by the end user
to build assembly programs. Every other pass is something the user does not need to worry
about. The first pass translates the DSL into our internal Seq.Seq Statement.
-}
type Assembly a = Hane Assemble Assemble a

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

-- | The list of Statements is constructed in order, so the O(1) append helps a ton.
data Assemble = Assemble {assemble :: Seq.Seq Statement}

data Resolve = Resolve
  { resolveStatements :: Seq.Seq Statement
  , resolveLabels :: Seq.Seq (Word16, T.Text)
  , resolveCodegen :: Seq.Seq T.Text
  , resolveProgramCounter :: Int
  }

data Env = Env
  { assembler :: Bool
  , emulator :: Bool
  , display :: Bool
  , logLevel :: LogLevel
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
