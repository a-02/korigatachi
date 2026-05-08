{-# LANGUAGE PatternSynonyms #-}

module Korigatachi.Assembly.Pattern where

import Korigatachi.Assembly.Operand
import Korigatachi.Types

-- TIA Registers
pattern VSYNC :: Operand
pattern VSYNC = ZeroPage 0x00 -- Vertical Sync Set-Clear
pattern VBLANK :: Operand
pattern VBLANK = ZeroPage 0x01 -- Vertical Blank Set-Clear
pattern WSYNC :: Operand
pattern WSYNC = ZeroPage 0x02 -- Wait for Horizontal Blank
pattern RSYNC :: Operand
pattern RSYNC = ZeroPage 0x03 -- Reset Horizontal Sync Counter
pattern NUSIZ0 :: Operand
pattern NUSIZ0 = ZeroPage 0x04 -- Number-Size player/missle 0
pattern NUSIZ1 :: Operand
pattern NUSIZ1 = ZeroPage 0x05 -- Number-Size player/missle 1
pattern COLUP0 :: Operand
pattern COLUP0 = ZeroPage 0x06 -- Color-Luminance Player 0
pattern COLUP1 :: Operand
pattern COLUP1 = ZeroPage 0x07 -- Color-Luminance Player 1
pattern COLUPF :: Operand
pattern COLUPF = ZeroPage 0x08 -- Color-Luminance Playfield
pattern COLUBK :: Operand
pattern COLUBK = ZeroPage 0x09 -- Color-Luminance Background
pattern CTRLPF :: Operand
pattern CTRLPF = ZeroPage 0x0A -- Control Playfield, Ball, Collisions
pattern REFP0 :: Operand
pattern REFP0 = ZeroPage 0x0B -- Reflection Player 0
pattern REFP1 :: Operand
pattern REFP1 = ZeroPage 0x0C -- Reflection Player 1
pattern PF0 :: Operand
pattern PF0 = ZeroPage 0x0D -- Playfield Register Byte 0
pattern PF1 :: Operand
pattern PF1 = ZeroPage 0x0E -- Playfield Register Byte 1
pattern PF2 :: Operand
pattern PF2 = ZeroPage 0x0F -- Playfield Register Byte 2
pattern RESP0 :: Operand
pattern RESP0 = ZeroPage 0x10 -- Reset Player 0
pattern RESP1 :: Operand
pattern RESP1 = ZeroPage 0x11 -- Reset Player 1
pattern RESM0 :: Operand
pattern RESM0 = ZeroPage 0x12 -- Reset Missle 0
pattern RESM1 :: Operand
pattern RESM1 = ZeroPage 0x13 -- Reset Missle 1
pattern RESBL :: Operand
pattern RESBL = ZeroPage 0x14 -- Reset Ball
pattern AUDC0 :: Operand
pattern AUDC0 = ZeroPage 0x15 -- Audio Control 0
pattern AUDC1 :: Operand
pattern AUDC1 = ZeroPage 0x16 -- Audio Control 1
pattern AUDF0 :: Operand
pattern AUDF0 = ZeroPage 0x17 -- Audio Frequency 0
pattern AUDF1 :: Operand
pattern AUDF1 = ZeroPage 0x18 -- Audio Frequency 1
pattern AUDV0 :: Operand
pattern AUDV0 = ZeroPage 0x19 -- Audio Volume 0
pattern AUDV1 :: Operand
pattern AUDV1 = ZeroPage 0x1A -- Audio Volume 1
pattern GRP0 :: Operand
pattern GRP0 = ZeroPage 0x1B -- Graphics Register Player 0
pattern GRP1 :: Operand
pattern GRP1 = ZeroPage 0x1C -- Graphics Register Player 1
pattern ENAM0 :: Operand
pattern ENAM0 = ZeroPage 0x1D -- Graphics Enable Missle 0
pattern ENAM1 :: Operand
pattern ENAM1 = ZeroPage 0x1E -- Graphics Enable Missle 1
pattern ENABL :: Operand
pattern ENABL = ZeroPage 0x1F -- Graphics Enable Ball
pattern HMP0 :: Operand
pattern HMP0 = ZeroPage 0x20 -- Horizontal Motion Player 0
pattern HMP1 :: Operand
pattern HMP1 = ZeroPage 0x21 -- Horizontal Motion Player 1
pattern HMM0 :: Operand
pattern HMM0 = ZeroPage 0x22 -- Horizontal Motion Missle 0
pattern HMM1 :: Operand
pattern HMM1 = ZeroPage 0x23 -- Horizontal Motion Missle 1
pattern HMBL :: Operand
pattern HMBL = ZeroPage 0x24 -- Horizontal Motion Ball
pattern VDELP0 :: Operand
pattern VDELP0 = ZeroPage 0x25 -- Vertical Delay Player 0
pattern VDELP1 :: Operand
pattern VDELP1 = ZeroPage 0x26 -- Vertical Delay Player 1
pattern VDELBL :: Operand
pattern VDELBL = ZeroPage 0x27 -- Vertical Delay Ball
pattern RESMP0 :: Operand
pattern RESMP0 = ZeroPage 0x28 -- Reset Missle 0 to Player 0
pattern RESMP1 :: Operand
pattern RESMP1 = ZeroPage 0x29 -- Reset Missle 1 to Player 1
pattern HMOVE :: Operand
pattern HMOVE = ZeroPage 0x2A -- Apply Horizontal Motion
pattern HMCLR :: Operand
pattern HMCLR = ZeroPage 0x2B -- Clear Horizontal Move Registers
pattern CXCLR :: Operand
pattern CXCLR = ZeroPage 0x2C -- Clear Collision Latches

-- PIA Registers
pattern SWCHA :: Operand
pattern SWCHA = Absolute 0x02 0x80
pattern SWACNT :: Operand
pattern SWACNT = Absolute 0x02 0x81
pattern SWCHB :: Operand
pattern SWCHB = Absolute 0x02 0x82
pattern SWBCNT :: Operand
pattern SWBCNT = Absolute 0x02 0x83
pattern INTIM :: Operand
pattern INTIM = Absolute 0x02 0x84
pattern TIM1T :: Operand
pattern TIM1T = Absolute 0x02 0x94
pattern TIM8T :: Operand
pattern TIM8T = Absolute 0x02 0x95
pattern TIM64T :: Operand
pattern TIM64T = Absolute 0x02 0x96
pattern T1024T :: Operand
pattern T1024T = Absolute 0x02 0x97
