{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}

module Main where

import Control.Monad (void)

import Korigatachi.Assembly.Control
import Korigatachi.Assembly.Instruction
import Korigatachi.Assembly.Pattern
import Korigatachi.Monad qualified as K
import Korigatachi.Types
import Korigatachi qualified as Korigatachi

{--

;---------------------------------------------------------------------------

;   * Subject: [stella] 2600 Digital Clock (source code)
;   * From: crackers@hwcn.org
;   * Date: Sun, 5 Oct 1997 14:31:50 -0400 (EDT)

;---------------------------------------------------------------------------

; Here's the source code for that final digital clock programme.
; Feel free to employ and distribute this code however you may wish.
; Both the source code and the binary are public domain.

; -----------------------------------------------------------------------------

--}
-- 
main :: IO ()
main = do
  void $ Korigatachi.render clock
  void $ Korigatachi.assemble clock

clock :: Assembly ()
clock = K.do
  preamble
  org 0xF000
  start
  zero
  mainLoop
  vertb
  time
  min0
  min5
  min4
  min3
  min2
  min1
  minload
  msload
  hour0
  hour1
  hour2
  loadhrs
  hsload
  numblk
  draw
  blow1
  sload
  blow2
  clear
  joy
  up
  now1
  down
  now2
  left
  now3
  right
  now4
  oscan
  zeros
  ones
  twos
  three
  fours
  fives
  sixes
  sevens
  eights
  nines
  org 0xFFFC
  word "start"
  word "start"


-- RAM

pattern TEMP = "$80"
pattern SECS = "$82"
pattern MINS = "$83"
pattern HOURS = "$84"
pattern JOYDEL = "$85"
pattern JOY1ST = "$86"
pattern SPRITEA = "$87"
pattern SPRITEB = "$8F"
pattern RMINS = "$97"
pattern RHOURS = "$98"
pattern FRAMES = "$99"

start :: Assembly ()
start = K.do
  label "Start"
  sei
  cld
  ldx "#$FF"
  txs
  lda "#$00"

zero :: Assembly ()
zero = K.do
  label "zero"
  sta "$00,X"
  dex
  bne "zero"
  lda "#$01"
  sta CTRLPF
  lda "#$0C"
  sta HOURS
  lda "#$3C"
  sta MINS
  lda "#$CA"
  sta COLUP0
  sta COLUP1
  lda "#$07"
  sta NUSIZ0
  sta NUSIZ1
  lda "#$3C"
  sta FRAMES
  sta SECS

mainLoop :: Assembly ()
mainLoop = K.do
  label "mainLoop"
  jsr "vertb"
  jsr "time"
  jsr "draw"
  jsr "clear"
  jmp "mainLoop"

vertb :: Assembly ()
vertb = K.do
  label "vertb"
  ldx "#$00"
  lda "#$02"
  rep 3 (sta WSYNC)
  sta VSYNC
  rep 2 (sta WSYNC)
  lda "#$2C"
  sta TIM64T
  lda "#$00"
  sta WSYNC
  sta VSYNC
  rts

time :: Assembly ()
time = K.do
  ldy "#06"
  lda "#$3C"
  sec
  sbc MINS
  sta RMINS
  cmp "#$00"
  beq "min0"
  cmp "#$32"
  bpl "min5"
  cmp "#$28"
  bpl "min4"
  cmp "#$1E"
  bpl "min3"
  cmp "#$14"
  bpl "min2"
  cmp "#$0A"
  bpl "min1"

min0 :: Assembly ()
min0 = K.do
  label "min0"
  lda "zeros,y"
  and "#$F0"
  sta (SPRITEA <> ",y")
  dey
  bpl "min0"
  lda "#$00"
  jmp "minload"

min5 :: Assembly ()
min5 = K.do
  label "min5"
  lda "fives,y"
  and "#$F0"
  sta (SPRITEA <> ",y")
  dey
  bpl "min5"
  lda "#$32"
  jmp "minload"
  
min4 :: Assembly ()
min4 = K.do
  label "min4"
  lda "fours,y"
  and "#$F0"
  sta (SPRITEA <> ",y")
  dey
  bpl "min4"
  lda "#$28"
  jmp "minload"
  
min3 :: Assembly ()
min3 = K.do
  label "min3"
  lda "threes,y"
  and "#$F0"
  sta (SPRITEA <> ",y")
  dey
  bpl "min3"
  lda "#$1E"
  jmp "minload"

min2 :: Assembly ()
min2 = K.do
  label "min2"
  lda "twos,y"
  and "#$F0"
  sta (SPRITEA <> ",y")
  dey
  bpl "min2"
  lda "#$14"
  jmp "minload"
  
min1 :: Assembly ()
min1 = K.do
  label "min1"
  lda "ones,y"
  and "#$F0"
  sta (SPRITEA <> ",y")
  dey
  bpl "min1"
  lda "#$0A"

minload :: Assembly ()
minload = K.do
  label "minload"
  sta TEMP
  lda RMINS
  sec
  sbc TEMP
  asl
  tax
  lda "numblk,x"
  sta TEMP
  lda "numblk+1,x"
  sta (TEMP + 1)
  ldy 
  
msload :: Assembly ()
msload = K.do
  label "msload"
  lda "(TEMP),y"
  and "#$0F"
  ora (SPRITEA <> ",y")
  sta (SPRITEA <> ",y")
  dey
  bpl "msload"
  ldy "#$06"
  lda "#$18"
  sec
  sbc HOURS
  sta RHOURS
  cmp "#$00"
  beq "hour0"
  cmp "#$14"
  bpl "hour2"
  cmp "#$0A"
  bpl "hour1"

hour0 :: Assembly ()
hour0 = K.do
  label "hour0"
  lda "zeros,y"
  and "#$F0"
  sta (SPRITEB <> ",y")
  dey
  bpl "hour0"
  lda "#$00"
  jmp "loadhrs"

hour1 :: Assembly ()
hour1 = K.do
  label "hour1"
  lda "ones,y"
  and "#$F0"
  sta (SPRITEB <> ",y")
  dey
  bpl "hour1"
  lda "#$0A"
  jmp "loadhrs"

hour2 :: Assembly ()
hour2 = K.do
  label "hour2"
  lda "twos,y"
  and "#$F0"
  sta (SPRITEB <> ",y")
  dey
  bpl "hour2"
  lda "#$14"
  jmp "loadhrs"

loadhrs :: Assembly ()
loadhrs = K.do
  label "loadhrs"
  sta TEMP
  lda RHOURS
  sec
  sbc TEMP
  asl
  tax
  lda "numblk,x"
  sta TEMP
  lda "numblk+1,x"
  sta TEMP+1
  ldy "#$06"

hsload :: Assembly ()
hsload = K.do
  label "hsload"
  lda "(TEMP),y"
  and "#$0F"
  ora (SPRITEB <> ",y")
  sta (SPRITEB <> ",y")
  dey
  bpl "hsload"
  rts

numblk :: Assembly ()
numblk = K.do
  label "numblk"
  word "zeros"
  word "ones"
  word "twos"
  word "threes"
  word "fours"
  word "fives"
  word "sixes"
  word "sevens"
  word "eights"
  word "nines"

draw :: Assembly ()
draw = K.do
  label "draw"
  lda INTIM
  bne "draw"
  sta WSYNC
  sta HMOVE
  sta VBLANK
  ldx "#$3F"

blow1 :: Assembly ()
blow1 = K.do
  label "blow1"
  sta WSYNC
  dex
  bpl "blow1"
  sta WSYNC
  rep 15 nop
  sta RESP0
  rep 7 nop
  sta RESP1
  ldy "#$06"

sload :: Assembly ()
sload = K.do
  label "sload"
  lda (SPRITEB <> ",y")
  sta GP0
  lda (SPRITEA <> ",y")
  sta GP1
  rep 8 (sta WSYNC)
  dey
  bpl "sload"
  lda "#$00"
  sta GP0
  sta GP1
  ldx "#$40"

blow2 :: Assembly ()
blow2 = K.do
  label "blow2"
  sta WSYNC
  dex
  bpl "blow2"
  rts

clear :: Assembly ()
clear = K.do
  label "clear"
  lda "#$24"
  sta TIM64T
  lda "#$02"
  sta WSYNC
  sta VBLANK
  lda "#$00"
  sta PF0
  sta PF1
  sta PF2
  sta COLUPF
  sta COLUBK
  lda "#3C"
  dec FRAMES
  bne "joy"
  sta SECS
  dec SECS
  dec MINS
  bne "joy"
  sta MINS
  lda "#$18"
  inc SECS
  dec HOURS
  bne "joy"
  sta HOURS

joy :: Assembly ()
joy = K.do
  label "up"
  lda SWCHA
  ora "#$0F"
  cmp "#$EF"
  beq "up"
  cmp "#$DF"
  beq "down"
  cmp "#$BF"
  beq "left"
  cmp "#$7F"
  beq "right"
  lda "#$00"
  sta JOYDEL
  lda "#$01"
  sta JOY1ST
  jmp "oscan"

up :: Assembly ()
up = K.do
  label "up"
  lda HOURS
  cmp "#$01"
  beq "oscan"
  inc JOYDEL
  lda JOY1ST
  cmp "#$01"
  beq "now1"
  lda "#$1E"
  cmp JOYDEL
  bne "oscan"

now1 :: Assembly ()
now1 = K.do
  label "now1"
  lda "#$00"
  sta JOY1ST
  sta JOYDEL
  dec HOURS
  jmp "oscan"

down :: Assemblu ()
down = K.do
  label "down"
  lda HOURS
  cmp "#$18"
  beq "oscan"
  inc JOYDEL
  lda JOY1ST
  cmp "#$01"
  beq "now2"
  lda JOYDEL
  cmp "#$1E"
  bne "oscan"

now2 :: Assembly ()
now2 = K.do
  label "now2"
  lda "#$00"
  sta JOY1ST
  sta JOYDEL
  inc HOURS
  jmp "oscan"

left :: Assemblu ()
left = K.do
  label "left"
  lda MINS
  cmp "#$01"
  beq "oscan"
  inc JOYDEL
  lda JOY1ST
  cmp "#$01"
  beq "now3"
  lda "#$1E"
  cmp JOYDEL
  bne "oscan"

now3 :: Assembly ()
now3 = K.do
  label "now3"
  lda "#$00"
  sta JOY1ST
  sta JOYDEL
  dec MINS
  jmp "oscan"
  
right :: Assemblu ()
right = K.do
  label "right"
  lda MINS
  cmp "#$01"
  beq "oscan"
  inc JOYDEL
  lda JOY1ST
  cmp "#$01"
  beq "now3"
  lda "#$1E"
  cmp JOYDEL
  bne "oscan"

now4 :: Assembly ()
now4 = K.do
  label "now4"
  lda "#$00"
  sta JOY1ST
  sta JOYDEL
  inc MINS
  
oscan :: Assembly ()
oscan = K.do
  lda INTIM
  bne "oscan"
  sta WSYNC
  rts

zeros :: Assembly ()
zeros = K.do
  label "zeros"
  byte 0b11100111
  byte 0b10100101
  byte 0b10100101
  byte 0b10100101
  byte 0b10100101
  byte 0b10100101
  byte 0b11100111

ones :: Assembly ()
ones = K.do
  label "ones"
  byte 0b11100111
  byte 0b01000010
  byte 0b01000010
  byte 0b01000010
  byte 0b01000010
  byte 0b11000110
  byte 0b01000010

twos :: Assembly ()
twos = K.do
  label "twos"
  byte 0b11100111
  byte 0b10000100
  byte 0b10000100
  byte 0b11100111
  byte 0b00100001
  byte 0b00100001
  byte 0b11100111

threes :: Assembly ()
threes = K.do
  label "threes"
  byte 0b11100111
  byte 0b00100001
  byte 0b00100001
  byte 0b11100111
  byte 0b00100001
  byte 0b00100001
  byte 0b11100111

fours :: Assembly ()
fours = K.do
  label "fours"
  byte 0b00100001
  byte 0b00100001
  byte 0b00100001
  byte 0b11100111
  byte 0b10100101
  byte 0b10100101
  byte 0b10000100

fives :: Assembly ()
fives = K.do
  label "fives"
  byte 0b11100111
  byte 0b00100001
  byte 0b00100001
  byte 0b11100111
  byte 0b10000100
  byte 0b10000100
  byte 0b11100111

sixes :: Assembly ()
sixes = K.do
  label "sixes"
  byte 0b11100111
  byte 0b10100101
  byte 0b10100101
  byte 0b11100111
  byte 0b10000100
  byte 0b10000100
  byte 0b11000110

sevens :: Assembly ()
sevens = K.do
  label "sevens"
  byte 0b10000100
  byte 0b10000100
  byte 0b10000100
  byte 0b01000010
  byte 0b00100001
  byte 0b00100001
  byte 0b11100111
  
eights :: Assembly ()
eights = K.do
  label "eights"
  byte 0b11100111
  byte 0b10100101
  byte 0b10100101
  byte 0b11100111
  byte 0b10100101
  byte 0b10100101
  byte 0b11100111

nines :: Assembly ()
nines = K.do
  label "nines"
  byte 0b00100001
  byte 0b00100001
  byte 0b00100001
  byte 0b11100111
  byte 0b10100101
  byte 0b10100101
  byte 0b11100111
  
