{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Use $>" -}

module Korigatachi.Assembly.Control where

import Data.Word (Word32, Word8)
import Prelude hiding (and, read)

import Data.Sequence qualified as Seq
import Data.Text qualified as T

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

rep :: K.ASMArg K.IntLike -> K.Hane i i () -> K.Hane i i ()
rep asmArg hane =
  go hane (K.destructIntLike asmArg)
 where
  go h i =
    case i of
      0 -> pure ()
      _ -> K.do
        h
        go h (i - 1)

org :: K.ASMArg a -> K.Assembly ()
org = \case
  K.AAByte b -> K.do
    K.log K.Note (orgUndersizeWarning b)
    append $ K.Org (fromIntegral b)
  K.AAWord w -> append $ K.Org w
  K.AALong l -> K.log K.Warn (orgOversizeWarning l)
  K.AAInt i
    | abs i < 2 ^ (8 :: Integer) -> org (K.AAByte $ fromIntegral i)
    | abs i < 2 ^ (16 :: Integer) -> org (K.AAWord $ fromIntegral i)
    | abs i < 2 ^ (32 :: Integer) -> org (K.AALong $ fromIntegral i)
    | otherwise -> org (K.AALong $ fromIntegral i)
  K.AAExpr _ -> undefined

word :: K.ASMArg a -> K.Assembly ()
word = \case
  K.AAByte b -> K.do
    K.log K.Note (wordUndersizeWarning b)
    append $ K.Word (fromIntegral b)
  K.AAWord w ->
    append $ K.Word w
  K.AALong l -> K.do
    K.log K.Note (wordOversizeWarning l)
    let (llll, hhhh) = K.splitWord32asWord16 l
    append $ K.Word llll
    append $ K.Word hhhh
  K.AAInt i
    | abs i < 2 ^ (8 :: Integer) -> word (K.AAByte $ fromIntegral i)
    | abs i < 2 ^ (16 :: Integer) -> word (K.AAWord $ fromIntegral i)
    | abs i < 2 ^ (32 :: Integer) -> word (K.AALong $ fromIntegral i)
    | otherwise -> org (K.AALong $ fromIntegral i)
  K.AAExpr _ -> undefined

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

label :: K.ASMArg K.TextLike -> K.Assembly ()
label = \case
  K.AAExpr expr -> append $ K.TopLevelLabel expr
  _ -> pure ()

val :: K.ASMArg K.TextLike -> K.ASMArg K.IntLike -> K.Assembly ()
val name value = do
  let nameText = K.destructTextLike name
      valueWord = K.destructIntLike value
  append $ K.Val nameText valueWord

orgUndersizeWarning :: Word8 -> T.Text
orgUndersizeWarning w8 = "`org` called with 8-bit arg \"" <> T.show w8 <> "\", expected 16-bit."

wordUndersizeWarning :: Word8 -> T.Text
wordUndersizeWarning w8 = "`word` called with 8-bit arg \"" <> T.show w8 <> "\", expected 16-bit."

orgOversizeWarning :: Word32 -> T.Text
orgOversizeWarning w32 =
  "`org` called with 32-bit arg \""
    <> T.show w32
    <> "\", expected 16-bit. Ignoring!"

wordOversizeWarning :: Word32 -> T.Text
wordOversizeWarning w32 =
  "`word` called with 32-bit arg \""
    <> T.show w32
    <> "\", expected 16-bit. Writing as two 16-bit words."
