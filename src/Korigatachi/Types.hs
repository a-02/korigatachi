{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Korigatachi.Types
  ( Operand (..)
  , Statement (..)
  , Hane
  , Assembly
  , Katteyomi (..)
  , Assemble (..)
  , Resolve (..)
  , Bin (..)
  , Env (..)
  , LogLevel (..)
  , BaseRepresentation (..)
  , Length (..)
  , ASMArg (..)
  , IntLike
  , TextLike
  , module Atari.Types.Export
  , module Resolve.Types.Export
  )
where

import Data.Word (Word16, Word32, Word8)
import Prelude hiding (break)

import Data.ByteString qualified as BS
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import GHC.Generics qualified as Generic

import Korigatachi.Atari.Types as Atari.Types.Export
import Korigatachi.Monad
import Korigatachi.Resolve.Types as Resolve.Types.Export

-- | Something.
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
  deriving (Show)

data Resolve = Resolve
  { resolveStatements :: Seq.Seq Statement
  , resolveLabels :: Seq.Seq (Word16, T.Text)
  , resolveCodegen :: Seq.Seq T.Text
  , resolveProgramCounter :: Word16
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

data ASMArg phantom where
  AAByte :: Word8 -> ASMArg IntLike
  AAWord :: Word16 -> ASMArg IntLike
  AALong :: Word32 -> ASMArg IntLike
  AAInt :: Integer -> ASMArg IntLike
  AAExpr :: T.Text -> ASMArg TextLike
  
-- instance Num (ASMArg IntLike) where
  -- (+) :: ASMArg a -> ASMArg a -> ASMArg a
  -- (+) = _
  -- (-) :: ASMArg a -> ASMArg a -> ASMArg a
  -- (-) = _
  -- (*) :: ASMArg a -> ASMArg a -> ASMArg a
  -- (*) = _
  -- negate :: ASMArg a -> ASMArg a
  -- negate = _
  -- abs :: ASMArg a -> ASMArg a
  -- abs = _
  -- signum :: ASMArg a -> ASMArg a
  -- signum = _
  -- fromInteger :: Integer -> ASMArg a
  -- fromInteger = _

-- data Expr =
