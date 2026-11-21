{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Korigatachi.Assembly.Pattern where

import Korigatachi.Types qualified as K

-- TIA Registers

-- | Vertical Sync Set-Clear
pattern VSYNC :: K.ASMArg K.TextLike
pattern VSYNC = K.AAExpr "$00"

-- | Vertical Blank Set-Clear
pattern VBLANK :: K.ASMArg K.TextLike
pattern VBLANK = K.AAExpr "$01"

-- | Wait for Horizontal Blank
pattern WSYNC :: K.ASMArg K.TextLike
pattern WSYNC = K.AAExpr "$02"

-- | Reset Horizontal Sync Counter
pattern RSYNC :: K.ASMArg K.TextLike
pattern RSYNC = K.AAExpr "$03"

-- | Number-Size player/missle 0
pattern NUSIZ0 :: K.ASMArg K.TextLike
pattern NUSIZ0 = K.AAExpr "$04"

-- | Number-Size player/missle 1
pattern NUSIZ1 :: K.ASMArg K.TextLike
pattern NUSIZ1 = K.AAExpr "$05"

-- | Color-Luminance Player 0
pattern COLUP0 :: K.ASMArg K.TextLike
pattern COLUP0 = K.AAExpr "$06"

-- | Color-Luminance Player 1
pattern COLUP1 :: K.ASMArg K.TextLike
pattern COLUP1 = K.AAExpr "$07"

-- | Color-Luminance Playfield
pattern COLUPF :: K.ASMArg K.TextLike
pattern COLUPF = K.AAExpr "$08"

-- | Color-Luminance Background
pattern COLUBK :: K.ASMArg K.TextLike
pattern COLUBK = K.AAExpr "$09"

-- | Control Playfield, Ball, Collisions
pattern CTRLPF :: K.ASMArg K.TextLike
pattern CTRLPF = K.AAExpr "$0A"

-- | Reflection Player 0
pattern REFP0 :: K.ASMArg K.TextLike
pattern REFP0 = K.AAExpr "$0B"

-- | Reflection Player 1
pattern REFP1 :: K.ASMArg K.TextLike
pattern REFP1 = K.AAExpr "$0C"

-- | Playfield Register Byte 0
pattern PF0 :: K.ASMArg K.TextLike
pattern PF0 = K.AAExpr "$0D"

-- | Playfield Register Byte 1
pattern PF1 :: K.ASMArg K.TextLike
pattern PF1 = K.AAExpr "$0E"

-- | Playfield Register Byte 2
pattern PF2 :: K.ASMArg K.TextLike
pattern PF2 = K.AAExpr "$0F"

-- | Reset Player 0
pattern RESP0 :: K.ASMArg K.TextLike
pattern RESP0 = K.AAExpr "$10"

-- | Reset Player 1
pattern RESP1 :: K.ASMArg K.TextLike
pattern RESP1 = K.AAExpr "$11"

-- | Reset Missle 0
pattern RESM0 :: K.ASMArg K.TextLike
pattern RESM0 = K.AAExpr "$12"

-- | Reset Missle 1
pattern RESM1 :: K.ASMArg K.TextLike
pattern RESM1 = K.AAExpr "$13"

-- | Reset Ball
pattern RESBL :: K.ASMArg K.TextLike
pattern RESBL = K.AAExpr "$14"

-- | Audio Control 0
pattern AUDC0 :: K.ASMArg K.TextLike
pattern AUDC0 = K.AAExpr "$15"

-- | Audio Control 1
pattern AUDC1 :: K.ASMArg K.TextLike
pattern AUDC1 = K.AAExpr "$16"

-- | Audio Frequency 0
pattern AUDF0 :: K.ASMArg K.TextLike
pattern AUDF0 = K.AAExpr "$17"

-- | Audio Frequency 1
pattern AUDF1 :: K.ASMArg K.TextLike
pattern AUDF1 = K.AAExpr "$18"

-- | Audio Volume 0
pattern AUDV0 :: K.ASMArg K.TextLike
pattern AUDV0 = K.AAExpr "$19"

-- | Audio Volume 1
pattern AUDV1 :: K.ASMArg K.TextLike
pattern AUDV1 = K.AAExpr "$1A"

-- | Graphics Register Player 0
pattern GRP0 :: K.ASMArg K.TextLike
pattern GRP0 = K.AAExpr "$1B"

-- | Graphics Register Player 1
pattern GRP1 :: K.ASMArg K.TextLike
pattern GRP1 = K.AAExpr "$1C"

-- | Graphics Enable Missle 0
pattern ENAM0 :: K.ASMArg K.TextLike
pattern ENAM0 = K.AAExpr "$1D"

-- | Graphics Enable Missle 1
pattern ENAM1 :: K.ASMArg K.TextLike
pattern ENAM1 = K.AAExpr "$1E"

-- | Graphics Enable Ball
pattern ENABL :: K.ASMArg K.TextLike
pattern ENABL = K.AAExpr "$1F"

-- | Horizontal Motion Player 0
pattern HMP0 :: K.ASMArg K.TextLike
pattern HMP0 = K.AAExpr "$20"

-- | Horizontal Motion Player 1
pattern HMP1 :: K.ASMArg K.TextLike
pattern HMP1 = K.AAExpr "$21"

-- | Horizontal Motion Missle 0
pattern HMM0 :: K.ASMArg K.TextLike
pattern HMM0 = K.AAExpr "$22"

-- | Horizontal Motion Missle 1
pattern HMM1 :: K.ASMArg K.TextLike
pattern HMM1 = K.AAExpr "$23"

-- | Horizontal Motion Ball
pattern HMBL :: K.ASMArg K.TextLike
pattern HMBL = K.AAExpr "$24"

-- | Vertical Delay Player 0
pattern VDELP0 :: K.ASMArg K.TextLike
pattern VDELP0 = K.AAExpr "$25"

-- | Vertical Delay Player 1
pattern VDELP1 :: K.ASMArg K.TextLike
pattern VDELP1 = K.AAExpr "$26"

-- | Vertical Delay Ball
pattern VDELBL :: K.ASMArg K.TextLike
pattern VDELBL = K.AAExpr "$27"

-- | Reset Missle 0 to Player 0
pattern RESMP0 :: K.ASMArg K.TextLike
pattern RESMP0 = K.AAExpr "$28"

-- | Reset Missle 1 to Player 1
pattern RESMP1 :: K.ASMArg K.TextLike
pattern RESMP1 = K.AAExpr "$29"

-- | Apply Horizontal Motion
pattern HMOVE :: K.ASMArg K.TextLike
pattern HMOVE = K.AAExpr "$2A"

-- | Clear Horizontal Move Registers
pattern HMCLR :: K.ASMArg K.TextLike
pattern HMCLR = K.AAExpr "$2B"

-- | Clear Collision Latches
pattern CXCLR :: K.ASMArg K.TextLike
pattern CXCLR = K.AAExpr "$2C"

-- | PIA Registers
pattern SWCHA :: K.ASMArg K.TextLike
pattern SWCHA = K.AAExpr "$280"

pattern SWACNT :: K.ASMArg K.TextLike
pattern SWACNT = K.AAExpr "$281"
pattern SWCHB :: K.ASMArg K.TextLike
pattern SWCHB = K.AAExpr "$282"
pattern SWBCNT :: K.ASMArg K.TextLike
pattern SWBCNT = K.AAExpr "$283"
pattern INTIM :: K.ASMArg K.TextLike
pattern INTIM = K.AAExpr "$284"
pattern TIM1T :: K.ASMArg K.TextLike
pattern TIM1T = K.AAExpr "$294"
pattern TIM8T :: K.ASMArg K.TextLike
pattern TIM8T = K.AAExpr "$295"
pattern TIM64T :: K.ASMArg K.TextLike
pattern TIM64T = K.AAExpr "$296"
pattern T1024T :: K.ASMArg K.TextLike
pattern T1024T = K.AAExpr "$297"
