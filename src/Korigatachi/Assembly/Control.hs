{-# LANGUAGE BinaryLiterals #-}
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

import Data.Foldable (traverse_)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word8)
import Korigatachi.Assembly.Operand qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K
import Prelude hiding (and, read)

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

class Directive d where
  dc :: K.Length -> d -> K.Assembly ()

instance Directive Word32 where
  dc K.DCByte w32 = K.do
    let
      (ll, lh, hl, hh) = K.splitWord32asWord8 w32
    append $ K.Byte ll
    append $ K.Byte lh
    append $ K.Byte hl
    append $ K.Byte hh
  dc K.DCWord w32 = K.do
    let
      (llll, hhhh) = K.splitWord32asWord16 w32
    append $ K.Word llll
    append $ K.Word hhhh
  dc K.DCLong w32 = append $ K.Long w32

instance Directive Word16 where
  dc K.DCByte w16 = K.do
    let
      (ll, hh) = K.splitWord16 w16
    append $ K.Byte ll
    append $ K.Byte hh
  dc K.DCWord w16 = append $ K.Word w16
  dc K.DCLong w16 = append $ K.Long (fromIntegral w16)

instance Directive Word8 where
  dc K.DCByte w8 = append $ K.Byte w8
  dc K.DCWord w8 = append $ K.Word (fromIntegral w8)
  dc K.DCLong w8 = append $ K.Long (fromIntegral w8)

instance Directive d => Directive [d] where
  dc = traverse_ . dc -- LOL

instance Directive T.Text where
  dc K.DCByte lb = append $ K.ByteLabel K.LabelAbsolute lb
  dc K.DCWord lb = append $ K.WordLabel K.LabelAbsolute lb
  dc K.DCLong lb = append $ K.LongLabel K.LabelAbsolute lb

org :: Word16 -> K.Assembly ()
org = append . K.Org

word :: Directive d => d -> K.Assembly ()
word = dc K.DCWord

byte :: Directive d => d -> K.Assembly ()
byte = dc K.DCByte

long :: Directive d => d -> K.Assembly ()
long = dc K.DCLong

label :: T.Text -> K.Assembly ()
label = append . K.TopLevelLabel
