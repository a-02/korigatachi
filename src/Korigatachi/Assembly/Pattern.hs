{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}

module Korigatachi.Assembly.Pattern where

import Korigatachi.Assembly.Rebind
import Korigatachi.Assembly.Control (val)
import Korigatachi.Types qualified as K

-- TIA Registers

-- | Vertical Sync Set-Clear
vsync :: K.Assembly ()
vsync = val "vsync" 0x00

pattern VSYNC :: K.ASMArg K.TextLike
pattern VSYNC =  "vsync"

-- | Vertical Blank Set-Clear
vblank :: K.Assembly ()
vblank = val "vblank" 0x01

pattern VBLANK :: K.ASMArg K.TextLike
pattern VBLANK =  "vblank"

-- | Wait for Horizontal Blank
wsync :: K.Assembly ()
wsync = val "wsync" 0x02

pattern WSYNC :: K.ASMArg K.TextLike
pattern WSYNC =  "wsync"

-- | Reset Horizontal Sync Counter
rsync :: K.Assembly ()
rsync = val "rsync" 0x03

pattern RSYNC :: K.ASMArg K.TextLike
pattern RSYNC =  "rsync"

-- | Number-Size player/missle 0
nusiz0 :: K.Assembly ()
nusiz0 = val "nusiz0" 0x04

pattern NUSIZ0 :: K.ASMArg K.TextLike
pattern NUSIZ0 =  "nusiz0"

-- | Number-Size player/missle 1
nusiz1 :: K.Assembly ()
nusiz1 = val "nusiz1" 0x05

pattern NUSIZ1 :: K.ASMArg K.TextLike
pattern NUSIZ1 =  "nusiz1"

-- | Color-Luminance Player 0
colup0 :: K.Assembly ()
colup0 = val "colup0" 0x06

pattern COLUP0 :: K.ASMArg K.TextLike
pattern COLUP0 =  "colup0"

-- | Color-Luminance Player 1
colup1 :: K.Assembly ()
colup1 = val "colup1" 0x07

pattern COLUP1 :: K.ASMArg K.TextLike
pattern COLUP1 =  "colup1"

-- | Color-Luminance Playfield
colupf :: K.Assembly ()
colupf = val "colupf" 0x08

pattern COLUPF :: K.ASMArg K.TextLike
pattern COLUPF =  "colupf"

-- | Color-Luminance Background
colubk :: K.Assembly ()
colubk = val "colubk" 0x09

pattern COLUBK :: K.ASMArg K.TextLike
pattern COLUBK =  "colubk"

-- | Control Playfield, Ball, Collisions
ctrlpf :: K.Assembly ()
ctrlpf = val "ctrlpf" 0x0A

pattern CTRLPF :: K.ASMArg K.TextLike
pattern CTRLPF =  "ctrlpf"

-- | Reflection Player 0
refp0 :: K.Assembly ()
refp0 = val "refp0" 0x0B

pattern REFP0 :: K.ASMArg K.TextLike
pattern REFP0 =  "refp0"

-- | Reflection Player 1
refp1 :: K.Assembly ()
refp1 = val "refp1" 0x0C

pattern REFP1 :: K.ASMArg K.TextLike
pattern REFP1 =  "refp1"

-- | Playfield Register Byte 0
pf0 :: K.Assembly ()
pf0 = val "pf0" 0x0D

pattern PF0 :: K.ASMArg K.TextLike
pattern PF0 =  "pf0"

-- | Playfield Register Byte 1
pf1 :: K.Assembly ()
pf1 = val "pf1" 0x0E

pattern PF1 :: K.ASMArg K.TextLike
pattern PF1 =  "pf1"

-- | Playfield Register Byte 2
pf2 :: K.Assembly ()
pf2 = val "pf2" 0x0F

pattern PF2 :: K.ASMArg K.TextLike
pattern PF2 =  "pf2"

-- | Reset Player 0
resp0 :: K.Assembly ()
resp0 = val "resp0" 0x10

pattern RESP0 :: K.ASMArg K.TextLike
pattern RESP0 =  "resp0"

-- | Reset Player 1
resp1 :: K.Assembly ()
resp1 = val "resp1" 0x11

pattern RESP1 :: K.ASMArg K.TextLike
pattern RESP1 =  "resp1"

-- | Reset Missle 0
resm0 :: K.Assembly ()
resm0 = val "resm0" 0x12

pattern RESM0 :: K.ASMArg K.TextLike
pattern RESM0 =  "resm0"

-- | Reset Missle 1
resm1 :: K.Assembly ()
resm1 = val "resm1" 0x13

pattern RESM1 :: K.ASMArg K.TextLike
pattern RESM1 =  "resm1"

-- | Reset Ball
resbl :: K.Assembly ()
resbl = val "resbl" 0x14

pattern RESBL :: K.ASMArg K.TextLike
pattern RESBL =  "resbl"

-- | Audio Control 0
audc0 :: K.Assembly ()
audc0 = val "audc0" 0x15

pattern AUDC0 :: K.ASMArg K.TextLike
pattern AUDC0 =  "audc0"

-- | Audio Control 1
audc1 :: K.Assembly ()
audc1 = val "audc1" 0x16

pattern AUDC1 :: K.ASMArg K.TextLike
pattern AUDC1 =  "audc1"

-- | Audio Frequency 0
audf0 :: K.Assembly ()
audf0 = val "audf0" 0x17

pattern AUDF0 :: K.ASMArg K.TextLike
pattern AUDF0 =  "audf0"

-- | Audio Frequency 1
audf1 :: K.Assembly ()
audf1 = val "audf1" 0x18

pattern AUDF1 :: K.ASMArg K.TextLike
pattern AUDF1 =  "audf1"

-- | Audio Volume 0
audv0 :: K.Assembly ()
audv0 = val "audv0" 0x19

pattern AUDV0 :: K.ASMArg K.TextLike
pattern AUDV0 =  "audv0"

-- | Audio Volume 1
audv1 :: K.Assembly ()
audv1 = val "audv1" 0x1A

pattern AUDV1 :: K.ASMArg K.TextLike
pattern AUDV1 =  "audv1"

-- | Graphics Register Player 0
grp0 :: K.Assembly ()
grp0 = val "grp0" 0x1B

pattern GRP0 :: K.ASMArg K.TextLike
pattern GRP0 =  "grp0"

-- | Graphics Register Player 1
grp1 :: K.Assembly ()
grp1 = val "grp1" 0x1C

pattern GRP1 :: K.ASMArg K.TextLike
pattern GRP1 =  "grp1"

-- | Graphics Enable Missle 0
enam0 :: K.Assembly ()
enam0 = val "enam0" 0x1D

pattern ENAM0 :: K.ASMArg K.TextLike
pattern ENAM0 =  "enam0"

-- | Graphics Enable Missle 1
enam1 :: K.Assembly ()
enam1 = val "enam1" 0x1E

pattern ENAM1 :: K.ASMArg K.TextLike
pattern ENAM1 =  "enam1"

-- | Graphics Enable Ball
enabl :: K.Assembly ()
enabl = val "enabl" 0x1F

pattern ENABL :: K.ASMArg K.TextLike
pattern ENABL =  "enabl"

-- | Horizontal Motion Player 0
hmp0 :: K.Assembly ()
hmp0 = val "hmp0" 0x20

pattern HMP0 :: K.ASMArg K.TextLike
pattern HMP0 =  "hmp0"

-- | Horizontal Motion Player 1
hmp1 :: K.Assembly ()
hmp1 = val "hmp1" 0x21

pattern HMP1 :: K.ASMArg K.TextLike
pattern HMP1 =  "hmp1"

-- | Horizontal Motion Missle 0
hmm0 :: K.Assembly ()
hmm0 = val "hmm0" 0x22

pattern HMM0 :: K.ASMArg K.TextLike
pattern HMM0 =  "hmm0"

-- | Horizontal Motion Missle 1
hmm1 :: K.Assembly ()
hmm1 = val "hmm1" 0x23

pattern HMM1 :: K.ASMArg K.TextLike
pattern HMM1 =  "hmm1"

-- | Horizontal Motion Ball
hmbl :: K.Assembly ()
hmbl = val "hmbl" 0x24

pattern HMBL :: K.ASMArg K.TextLike
pattern HMBL =  "hmbl"

-- | Vertical Delay Player 0
vdelp0 :: K.Assembly ()
vdelp0 = val "vdelp0" 0x25

pattern VDELP0 :: K.ASMArg K.TextLike
pattern VDELP0 =  "vdelp0"

-- | Vertical Delay Player 1
vdelp1 :: K.Assembly ()
vdelp1 = val "vdelp1" 0x26

pattern VDELP1 :: K.ASMArg K.TextLike
pattern VDELP1 =  "vdelp1"

-- | Vertical Delay Ball
vdelbl :: K.Assembly ()
vdelbl = val "vdelbl" 0x27

pattern VDELBL :: K.ASMArg K.TextLike
pattern VDELBL =  "vdelbl"

-- | Reset Missle 0 to Player 0
resmp0 :: K.Assembly ()
resmp0 = val "resmp0" 0x28

pattern RESMP0 :: K.ASMArg K.TextLike
pattern RESMP0 =  "resmp0"

-- | Reset Missle 1 to Player 1
resmp1 :: K.Assembly ()
resmp1 = val "resmp1" 0x29

pattern RESMP1 :: K.ASMArg K.TextLike
pattern RESMP1 =  "resmp1"

-- | Apply Horizontal Motion
hmove :: K.Assembly ()
hmove = val "hmove" 0x2A

pattern HMOVE :: K.ASMArg K.TextLike
pattern HMOVE =  "hmove"

-- | Clear Horizontal Move Registers
hmclr :: K.Assembly ()
hmclr = val "hmclr" 0x2B

pattern HMCLR :: K.ASMArg K.TextLike
pattern HMCLR =  "hmclr"

-- | Clear Collision Latches
cxclr :: K.Assembly ()
cxclr = val "cxclr" 0x2C

pattern CXCLR :: K.ASMArg K.TextLike
pattern CXCLR =  "cxclr"

-- | PIA Registers
swcha :: K.Assembly ()
swcha = val "swcha" 0x280

pattern SWCHA :: K.ASMArg K.TextLike
pattern SWCHA =  "swcha"

swacnt :: K.Assembly ()
swacnt = val "swacnt" 0x281

pattern SWACNT :: K.ASMArg K.TextLike
pattern SWACNT =  "swacnt"

swchb :: K.Assembly ()
swchb = val "swchb" 0x282

pattern SWCHB :: K.ASMArg K.TextLike
pattern SWCHB =  "swchb"

swbcnt :: K.Assembly ()
swbcnt = val "swbcnt" 0x283

pattern SWBCNT :: K.ASMArg K.TextLike
pattern SWBCNT =  "swbcnt"

intim :: K.Assembly ()
intim = val "intim" 0x284

pattern INTIM :: K.ASMArg K.TextLike
pattern INTIM =  "intim"

tim1t :: K.Assembly ()
tim1t = val "tim1t" 0x294

pattern TIM1T :: K.ASMArg K.TextLike
pattern TIM1T =  "tim1t"

tim8t :: K.Assembly ()
tim8t = val "tim8t" 0x295

pattern TIM8T :: K.ASMArg K.TextLike
pattern TIM8T =  "tim8t"

tim64t :: K.Assembly ()
tim64t = val "tim64t" 0x296

pattern TIM64T :: K.ASMArg K.TextLike
pattern TIM64T =  "tim64t"

t1024t :: K.Assembly ()
t1024t = val "t1024t" 0x297

pattern T1024T :: K.ASMArg K.TextLike
pattern T1024T =  "t1024t"
