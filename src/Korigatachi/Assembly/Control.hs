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
import Data.Word (Word16)
import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K
import Prelude hiding (and, read)

append :: K.Statement -> K.Assembly ()
append statement =
  K.modify $ \(K.Assemble s) -> K.Assemble $ s Seq.|> statement

-- {- | Assembly instructions are indented to semantically
-- seperate them from labels and other not-assembly things.
-- This is hard-coded to two spaces because it looks nice.
-- -}
-- spacing :: T.Text
-- spacing = "  "

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

label :: T.Text -> K.Assembly ()
label = append . K.TopLevelLabel

-- {- | Label a part of the ROM.
-- You can use this to refer to different sections of the program.
-- -}
-- label :: T.Text -> Korigatachi ()
-- label labelText = K.do
--   labelByte <- K.query $ \a -> a.rom.focus
--   K.modify $ #rom % #labels %~ (\ls -> K.MemoryLabel labelText labelByte : ls)
--   K.codeGen labelText

-- resolveLabel :: T.Text -> Korigatachi T.Text
-- resolveLabel labelText = K.do
--   atari <- K.get
--   let
--     labels = atari.rom.labels
--     shortCircuitFilter _ _ [] = 0
--     shortCircuitFilter f predicate (x : xs) =
--       if predicate x then f x else shortCircuitFilter f predicate xs
--   pure . T.show $ shortCircuitFilter K.labelByte (\(K.MemoryLabel memoryLabel _) -> memoryLabel == labelText) labels

-- {- | Move where the assembler writes bytes to.
-- org 0xFFFC will place the "focus" at the 2nd to last byte
-- in ROM.
-- -}
-- org :: Word16 -> Korigatachi ()
-- org w16 = K.do
--   K.modify $ #rom % #focus .~ (w16 .&. 0x0FFF)
--   K.codeGen (spacing <> "org $" <> (T.pack $ w16 ^. K.hex16))

-- -- | Place a 16-bit word in the ROM. Little-endian.
-- word :: Word16 -> Korigatachi ()
-- word w16 = K.do
--   let
--     (ll, hh) = splitWord16 w16
--   assembleROM ll
--   assembleROM hh
--   K.codeGen (spacing <> ".word $" <> (T.pack $ w16 ^. K.hex16))

-- advanceTV :: K.Instruction -> Korigatachi ()
-- advanceTV ins =
--   K.modify (\atari -> atari {K.tv = K.advanceTV ins.cycles atari.tv})

-- -- Weird trick: change the transition state and then just put it back later.
-- -- K.do
-- --   atari <- K.get
-- --   K.modify (const ())
-- --   K.modify (const atari)
