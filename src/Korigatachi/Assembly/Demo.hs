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

{- HLINT ignore "Use $>" -}

module Korigatachi.Assembly.Demo where

import Korigatachi.Assembly.Control
import Korigatachi.Assembly.Instruction
import Korigatachi.Assembly.Pattern
import Korigatachi.Monad qualified as K
import Korigatachi.Types

sample :: Assembly ()
sample = K.do
  preamble
  org 0xF000
  start
  clearMem
  mainLoop
  waitForVblankEnd
  scanLoop
  overScanWait
  org 0xFFFC
  word 0xF000
  word 0xF000

-- This is a direct translation of Kirk Israel's "thin red line".

-- | The standard Atari 2600 start script.
start :: Assembly ()
start = K.do
  label "Start"
  sei
  cld
  ldx "#$FF"
  txs
  lda "#$00"

clearMem :: Assembly ()
clearMem = K.do
  label "ClearMem"
  sta "(0,X)"
  dex
  bne "ClearMem"
  lda "#$00"
  sta SWACNT
  sta COLUBK
  lda "#33"
  sta COLUP0

mainLoop :: Assembly ()
mainLoop = K.do
  label "MainLoop"
  lda "#2"
  sta VSYNC -- TODO: Use this to showcase a replicator.
  sta WSYNC
  sta WSYNC
  sta WSYNC
  lda "#43"
  sta TIM64T
  lda "#0"
  sta VSYNC

waitForVblankEnd :: Assembly ()
waitForVblankEnd = K.do
  label "WaitForVblankEnd"
  lda INTIM
  bne "WaitForVblankEnd"
  ldy "#191"
  sta WSYNC
  sta VBLANK
  lda "#$F0"
  sta HMM0
  sta WSYNC
  sta HMOVE

scanLoop :: Assembly ()
scanLoop = K.do
  label "ScanLoop"
  lda SWCHA -- load joysticks
  sta COLUBK -- store as background
  sta WSYNC
  dey
  bne "ScanLoop"
  lda "#2"
  sta WSYNC
  sta VBLANK
  ldx "#30"

overScanWait :: Assembly ()
overScanWait = K.do
  label "OverScanWait"
  sta WSYNC
  dex
  bne "OverScanWait"
  jmp "MainLoop"
