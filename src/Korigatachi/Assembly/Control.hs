{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- HLINT ignore "Use $>" -}

module Korigatachi.Assembly.Control where

import Prelude hiding (and, read)

import Data.Sequence qualified as Seq

import Korigatachi.Assembly.Control.Internal qualified as K
import Korigatachi.Assembly.Operand qualified as K
import Korigatachi.Control qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K

append :: K.Statement -> K.Assembly ()
append statement =
  K.modify $ \(K.Assemble s) -> K.Assemble $ s Seq.|> statement

{- | The start of a valid Atari 2600 asm file.
Korigatachi is meant to not only assemble Atari machine code
itself, but also spit out assembly that is readable by the dasm
8-bit assembler.
-}
preamble :: K.Assembly ()
preamble = K.do
  append $ K.Processor "6502"
  append $ K.Include "vcs.h"

{- | Repeat a certain Hane an `IntLike` amount of times.
This is pretty inefficient manual recursion. Seek better alternatives!
-}
rep :: K.ASMArg K.IntLike -> K.Hane i i () -> K.Hane i i ()
rep asmArg hane =
  go hane (K.destructIntLike asmArg)
 where
  go h i =
    case i of
      0 -> pure ()
      _ -> K.do
        h
        go h (i Prelude.- 1)

{- | Place the program counter at a certain value.
This is traditionally 0xF000. It is unadvised to change the org unless you really know what you're doing.
NOTE: Setting the org to any value below 0x1000 is unadvised and should be avoided.
All labels are expected to be 16-bit and are resolved with 16-bit addressing modes.
If you need to specify an 8-bit value to refer to when writing your assembly, use `val` instead.
-}
org :: K.ASMArg a -> K.Assembly ()
org = \case
  K.AAByte b -> K.do
    K.log K.Note (K.orgUndersizeWarning b)
    append $ K.Org (fromIntegral b)
  K.AAWord w -> append $ K.Org w
  K.AALong l -> K.log K.Warn (K.orgOversizeWarning l)
  K.AAInt i
    | abs i < 2 ^ (8 :: Integer) -> org (K.AAByte $ fromIntegral i)
    | abs i < 2 ^ (16 :: Integer) -> org (K.AAWord $ fromIntegral i)
    | abs i < 2 ^ (32 :: Integer) -> org (K.AALong $ fromIntegral i)
    | otherwise -> org (K.AALong $ fromIntegral i)
  K.AAExpr _ -> undefined

-- Write one or more 16-bit unsigned values to the program.
-- Calling this with oversized values (32-bit, unbounded Integers) will lead to
-- multiple words being written to the program.
word :: K.ASMArg a -> K.Assembly ()
word = \case
  K.AAByte b -> K.do
    K.log K.Note (K.wordUndersizeWarning b)
    append $ K.Word (fromIntegral b)
  K.AAWord w ->
    append $ K.Word w
  K.AALong l -> K.do
    K.log K.Note (K.wordOversizeWarning l)
    let (llll, hhhh) = K.splitWord32asWord16 l
    append $ K.Word llll
    append $ K.Word hhhh
  K.AAInt i
    | abs i < 2 ^ (8 :: Integer) -> word (K.AAByte $ fromIntegral i)
    | abs i < 2 ^ (16 :: Integer) -> word (K.AAWord $ fromIntegral i)
    | abs i < 2 ^ (32 :: Integer) -> word (K.AALong $ fromIntegral i)
    | otherwise -> org (K.AALong $ fromIntegral i)
  K.AAExpr _ -> undefined

-- Write one or more 8-bit unsigned values to the program.
-- Calling this with oversized values (16-bit, 32-bit, unbounded Integers) will lead to
-- multiple bytes being written to the program.
byte :: K.ASMArg a -> K.Assembly ()
byte = \case
  K.AAByte b ->
    append $ K.Byte b
  K.AAWord w -> K.do
    let (ll, hh) = K.splitWord16 w
    append $ K.Byte ll
    append $ K.Byte hh
  K.AALong l -> K.do
    let (ll, lh, hl, hh) = K.splitWord32asWord8 l
    append $ K.Byte ll
    append $ K.Byte lh
    append $ K.Byte hl
    append $ K.Byte hh
  K.AAInt i
    | abs i < 2 ^ (8 :: Integer) -> word (K.AAByte $ fromIntegral i)
    | abs i < 2 ^ (16 :: Integer) -> word (K.AAWord $ fromIntegral i)
    | abs i < 2 ^ (32 :: Integer) -> word (K.AALong $ fromIntegral i)
    | otherwise -> org (K.AALong $ fromIntegral i)
  K.AAExpr _ -> undefined

-- | Write a 32-bit unsigned value to the program.
long :: K.ASMArg a -> K.Assembly ()
long = \case
  K.AAByte b ->
    append $ K.Long (fromIntegral b)
  K.AAWord w ->
    append $ K.Long (fromIntegral w)
  K.AALong l ->
    append $ K.Long l
  K.AAInt i ->
    append $ K.Long (fromIntegral i)
  K.AAExpr _ -> undefined

{- | A `label` is used to give a textual name to a certain location within the assembly program.
You can then use these labels to refer to specific portions of the code.
If you want to assign arbitrary values to arbitrary text, use `val`.
-}
label :: K.ASMArg K.TextLike -> K.Assembly ()
label = append . K.TopLevelLabel . K.destructTextLike

{- | A `val` is a value assigned to a name. This is used for specifying names for RAM addresses,
hardware ports, or whatever you want to refer to as text.
-}
val :: K.ASMArg K.TextLike -> K.ASMArg K.IntLike -> K.Assembly ()
val name value = append $ K.Val (K.destructTextLike name) (K.destructIntLike value)

(+), (*), (-) :: K.ASMOp
(+) = K.binaryASMOperation (Prelude.+) "+"
(-) = K.binaryASMOperation (Prelude.-) "-"
(*) = K.binaryASMOperation (Prelude.*) "*"

