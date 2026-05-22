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

module Korigatachi.Assembly where

-- base
import Data.List

-- insert-ordered-containers
import Data.HashMap.Strict.InsOrd qualified as InsOrd

-- optics
import Optics

-- text
import Data.Text qualified as T

-- korigatachi

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Bits (Bits ((.&.)))
import Data.List as List (find)
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Korigatachi.Assembly.Operand
import Korigatachi.Assembly.ReadWrite
import Korigatachi.Control qualified as K
import Korigatachi.Model qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types (Korigatachi, Operand (..), Shorthand (..))
import Korigatachi.Types qualified as K
import Prelude hiding (read)

spacing :: T.Text
spacing = "  "

preamble :: Korigatachi ()
preamble = K.do
  K.codeGen "  processor 6502"
  K.codeGen "  include vcs.h"

{- | Label a part of the ROM.
You can use this to refer to different sections of the program.
-}
label :: T.Text -> Korigatachi ()
label labelText = K.do
  labelByte <- K.query $ \a -> a.rom.focus
  K.modify $ #rom % #labels %~ (\ls -> K.MemoryLabel labelText labelByte : ls)
  K.codeGen labelText

{- | Move where the assembler writes bytes to.
org 0xFFFC will place the "focus" at the 2nd to last byte
in ROM.
-}
org :: Word16 -> Korigatachi ()
org w16 = K.do
  K.modify $ #rom % #focus .~ (w16 .&. 0x0FFF)
  K.codeGen (spacing <> "org $" <> (T.pack $ w16 ^. K.hex16))

-- | Place a 16-bit word in the ROM. Little-endian.
word :: Word16 -> Korigatachi ()
word w16 = K.do
  let
    (ll, hh) = splitWord16 w16
  assembleROMInternal ll
  assembleROMInternal hh
  K.codeGen (spacing <> ".word $" <> (T.pack $ w16 ^. K.hex16))

-- TODO: This is so fucking ugly.
instruct :: Shorthand -> T.Text -> (Operand -> Korigatachi ()) -> Korigatachi ()
instruct sh oprText emulate =
  K.do
    env <- K.ask
    let
      -- To avoid confusion between Relative and Implied addressing modes, we filter
      -- the possible addressing modes to parse an operand text based on the shorthand.
      -- We can do this since none of the members of validInstructions have the same shorthand
      -- but two entries with Relative and Implied addressing respectively.
      relevantAddressingModes = InsOrd.toList $ (\ins -> ins.addressingMode) <$> K.filterShorthand sh
      parser = foldl1' (<|>) $ addressingModeToParser . snd <$> relevantAddressingModes
      opr = fromMaybe (Label oprText) . Attoparsec.maybeResult $ Attoparsec.parse parser oprText
      bytecode = fromMaybe 255 $ fst <$> List.find (\(_, x) -> (toAddressingMode opr) == x) relevantAddressingModes
      instruction = InsOrd.lookupDefault K.jamInstruction bytecode K.validInstructions
    K.when env.assembler $ K.do
      K.log K.Info ("Assembling: " <> K.shorthandText sh <> " " <> oprText)
      K.log K.Test ("Relevant Addressing Modes: " <> T.show relevantAddressingModes)
      K.log K.Test ("Parsed operand: " <> T.show opr)
      K.log K.Test ("Bytecode: " <> T.show bytecode)
      K.log K.Test ("Instruction: " <> T.show instruction)
      K.codeGen (spacing <> K.shorthandText sh <> " " <> oprText)
      assembleROMInternal instruction.opcode
    K.when env.emulator $ (emulate opr)
    K.when env.display $ K.do
      advanceTV instruction

dex :: Korigatachi ()
dex = instruct DEX "" (\_ -> K.ixpure ()) -- TODO: Decrement X by one.

dey :: Korigatachi ()
dey = instruct DEX "" (\_ -> K.ixpure ()) -- TODO: Decrement Y by one.

bne :: T.Text -> Korigatachi ()
bne oprText = instruct BNE oprText (\_ -> K.ixpure ()) -- TODO: Break if non zero.

jmp :: T.Text -> Korigatachi ()
jmp oprText = instruct JMP oprText (\_ -> K.ixpure ()) -- TODO: Jump to instruction.

cld :: Korigatachi ()
cld = instruct CLD "" (\_ -> clearFlags 0b000010000)

lda :: T.Text -> Korigatachi ()
lda oprText = instruct LDA oprText $ \opr -> K.do
  val <- readOpr opr
  K.modify $ #cpu % #generalRegisters % #a .~ val

ldx :: T.Text -> Korigatachi ()
ldx oprText = instruct LDX oprText $ \opr -> K.do
  val <- readOpr opr
  K.modify $ #cpu % #generalRegisters % #x .~ val

ldy :: T.Text -> Korigatachi ()
ldy oprText = instruct LDY oprText $ \opr -> K.do
  val <- readOpr opr
  K.modify $ #cpu % #generalRegisters % #y .~ val

sei :: Korigatachi ()
sei = instruct SEI "" (\_ -> setFlags 0b00000100)

sta :: T.Text -> Korigatachi ()
sta oprText = instruct STA oprText $ \opr ->
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.a opr

stx :: T.Text -> Korigatachi ()
stx oprText = instruct STX oprText $ \opr ->
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.x opr

sty :: T.Text -> Korigatachi ()
sty oprText = instruct STX oprText $ \opr ->
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.y opr

txs :: Korigatachi ()
txs = instruct TXS "" $ \_ ->
  K.modify $
    \atari -> atari & #cpu % #stackPointer .~ (atari ^. #cpu % #generalRegisters % #x)

advanceTV :: K.Instruction -> Korigatachi ()
advanceTV ins =
  K.modify (\atari -> atari {K.tv = K.advanceTV ins.cycles atari.tv})

-- Weird trick: change the transition state and then just put it back later.
-- K.do
--   atari <- K.get
--   K.modify (const ())
--   K.modify (const atari)
