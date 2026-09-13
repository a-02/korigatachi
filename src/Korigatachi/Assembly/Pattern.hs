{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Korigatachi.Assembly.Pattern where

import Data.Text qualified as T

-- TIA Registers

-- | Vertical Sync Set-Clear
pattern VSYNC :: T.Text
pattern VSYNC = "$00"

-- | Vertical Blank Set-Clear
pattern VBLANK :: T.Text
pattern VBLANK = "$01"

-- | Wait for Horizontal Blank
pattern WSYNC :: T.Text
pattern WSYNC = "$02"

-- | Reset Horizontal Sync Counter
pattern RSYNC :: T.Text
pattern RSYNC = "$03"

-- | Number-Size player/missle 0
pattern NUSIZ0 :: T.Text
pattern NUSIZ0 = "$04"

-- | Number-Size player/missle 1
pattern NUSIZ1 :: T.Text
pattern NUSIZ1 = "$05"

-- | Color-Luminance Player 0
pattern COLUP0 :: T.Text
pattern COLUP0 = "$06"

-- | Color-Luminance Player 1
pattern COLUP1 :: T.Text
pattern COLUP1 = "$07"

-- | Color-Luminance Playfield
pattern COLUPF :: T.Text
pattern COLUPF = "$08"

-- | Color-Luminance Background
pattern COLUBK :: T.Text
pattern COLUBK = "$09"

-- | Control Playfield, Ball, Collisions
pattern CTRLPF :: T.Text
pattern CTRLPF = "$0A"

-- | Reflection Player 0
pattern REFP0 :: T.Text
pattern REFP0 = "$0B"

-- | Reflection Player 1
pattern REFP1 :: T.Text
pattern REFP1 = "$0C"

-- | Playfield Register Byte 0
pattern PF0 :: T.Text
pattern PF0 = "$0D"

-- | Playfield Register Byte 1
pattern PF1 :: T.Text
pattern PF1 = "$0E"

-- | Playfield Register Byte 2
pattern PF2 :: T.Text
pattern PF2 = "$0F"

-- | Reset Player 0
pattern RESP0 :: T.Text
pattern RESP0 = "$10"

-- | Reset Player 1
pattern RESP1 :: T.Text
pattern RESP1 = "$11"

-- | Reset Missle 0
pattern RESM0 :: T.Text
pattern RESM0 = "$12"

-- | Reset Missle 1
pattern RESM1 :: T.Text
pattern RESM1 = "$13"

-- | Reset Ball
pattern RESBL :: T.Text
pattern RESBL = "$14"

-- | Audio Control 0
pattern AUDC0 :: T.Text
pattern AUDC0 = "$15"

-- | Audio Control 1
pattern AUDC1 :: T.Text
pattern AUDC1 = "$16"

-- | Audio Frequency 0
pattern AUDF0 :: T.Text
pattern AUDF0 = "$17"

-- | Audio Frequency 1
pattern AUDF1 :: T.Text
pattern AUDF1 = "$18"

-- | Audio Volume 0
pattern AUDV0 :: T.Text
pattern AUDV0 = "$19"

-- | Audio Volume 1
pattern AUDV1 :: T.Text
pattern AUDV1 = "$1A"

-- | Graphics Register Player 0
pattern GRP0 :: T.Text
pattern GRP0 = "$1B"

-- | Graphics Register Player 1
pattern GRP1 :: T.Text
pattern GRP1 = "$1C"

-- | Graphics Enable Missle 0
pattern ENAM0 :: T.Text
pattern ENAM0 = "$1D"

-- | Graphics Enable Missle 1
pattern ENAM1 :: T.Text
pattern ENAM1 = "$1E"

-- | Graphics Enable Ball
pattern ENABL :: T.Text
pattern ENABL = "$1F"

-- | Horizontal Motion Player 0
pattern HMP0 :: T.Text
pattern HMP0 = "$20"

-- | Horizontal Motion Player 1
pattern HMP1 :: T.Text
pattern HMP1 = "$21"

-- | Horizontal Motion Missle 0
pattern HMM0 :: T.Text
pattern HMM0 = "$22"

-- | Horizontal Motion Missle 1
pattern HMM1 :: T.Text
pattern HMM1 = "$23"

-- | Horizontal Motion Ball
pattern HMBL :: T.Text
pattern HMBL = "$24"

-- | Vertical Delay Player 0
pattern VDELP0 :: T.Text
pattern VDELP0 = "$25"

-- | Vertical Delay Player 1
pattern VDELP1 :: T.Text
pattern VDELP1 = "$26"

-- | Vertical Delay Ball
pattern VDELBL :: T.Text
pattern VDELBL = "$27"

-- | Reset Missle 0 to Player 0
pattern RESMP0 :: T.Text
pattern RESMP0 = "$28"

-- | Reset Missle 1 to Player 1
pattern RESMP1 :: T.Text
pattern RESMP1 = "$29"

-- | Apply Horizontal Motion
pattern HMOVE :: T.Text
pattern HMOVE = "$2A"

-- | Clear Horizontal Move Registers
pattern HMCLR :: T.Text
pattern HMCLR = "$2B"

-- | Clear Collision Latches
pattern CXCLR :: T.Text
pattern CXCLR = "$2C"

-- | PIA Registers
pattern SWCHA :: T.Text
pattern SWCHA = "$280"

pattern SWACNT :: T.Text
pattern SWACNT = "$281"
pattern SWCHB :: T.Text
pattern SWCHB = "$282"
pattern SWBCNT :: T.Text
pattern SWBCNT = "$283"
pattern INTIM :: T.Text
pattern INTIM = "$284"
pattern TIM1T :: T.Text
pattern TIM1T = "$294"
pattern TIM8T :: T.Text
pattern TIM8T = "$295"
pattern TIM64T :: T.Text
pattern TIM64T = "$296"
pattern T1024T :: T.Text
pattern T1024T = "$297"
