{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}

module Korigatachi.Test.ThinRedLine where

import Data.Attoparsec.Text qualified as Attoparsec
import Test.Tasty
import Test.Tasty.HUnit qualified as HU
import Test.Tasty.Golden qualified as Golden

import Data.Either (isLeft)
import Korigatachi.Assembly.Operand qualified as K
import Korigatachi.Types qualified as K.Types
import Data.Sequence qualified as Seq
import Korigatachi.Bin qualified as K.Bin
import Korigatachi.Control
import Korigatachi.Monad qualified as K
import Korigatachi.Resolve qualified as K.Resolve
import Korigatachi.Types qualified as K
import Korigatachi.Assembly.Control
import Korigatachi.Assembly.Pattern
import Korigatachi.Types

thinRedLine :: K.Assembly ()
thinRedLine = K.do
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
start :: K.Assembly ()
start = K.do
  label "Start"
  sei
  cld
  ldx "#$FF"
  txs
  lda "#$00"

clearMem :: K.Assembly ()
clearMem = K.do
  label "ClearMem"
  sta "0,X" -- I wrote this wrong and spent hours trying to track down the bug this caused.
  dex
  bne "ClearMem"
  lda "#$00"
  sta SWACNT
  sta COLUBK
  lda "#33"
  sta COLUP0

mainLoop :: K.Assembly ()
mainLoop = K.do
  label "MainLoop"
  lda "#2"
  sta VSYNC
  rep 3 (sta WSYNC)
  lda "#43"
  sta TIM64T
  lda "#0"
  sta VSYNC

waitForVblankEnd :: K.Assembly ()
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

scanLoop :: K.Assembly ()
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

overScanWait :: K.Assembly ()
overScanWait = K.do
  label "OverScanWait"
  sta WSYNC
  dex
  bne "OverScanWait"
  jmp "MainLoop"
