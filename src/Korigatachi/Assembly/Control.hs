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

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Word (Word16, Word8)
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

org :: Word16 -> K.Assembly ()
org = append . K.Org

word :: Word16 -> K.Assembly ()
word = append . K.Word

byte :: Word8 -> K.Assembly ()
byte = append . K.Byte

label :: T.Text -> K.Assembly ()
label = append . K.TopLevelLabel
