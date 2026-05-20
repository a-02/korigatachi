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

import Data.Attoparsec.Text qualified as Attoparsec
import Data.Bits (Bits ((.&.)))
import Data.List as List (find)
import Data.Word (Word16)
import Korigatachi.Assembly.Operand
import Korigatachi.Assembly.ReadWrite
import Korigatachi.Control qualified as K
import Korigatachi.Model qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types (Korigatachi, Operand (..), Shorthand (..))
import Korigatachi.Types qualified as K
import Prelude hiding (read)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

{- | Label a part of the ROM.
You can use this to refer to different sections of the program.
-}
label :: T.Text -> Korigatachi ()
label labelText = K.do
  labelByte <- K.query $ \a -> a.rom.focus
  K.modify $ #rom % #labels %~ (\ls -> K.MemoryLabel labelText labelByte : ls)

org :: Word16 -> Korigatachi ()
org w16 = K.modify $ #rom % #focus .~ (w16 .&. 0x0FFF)

word :: Word16 -> Korigatachi ()
word w16 = K.do
  let
    (ll, hh) = splitWord16 w16
  assembleROMInternal ll
  assembleROMInternal hh


instruct :: Shorthand -> Operand -> Korigatachi () -> Korigatachi ()
instruct sh opr emulate =
  case lookupInstruction sh opr of
    Nothing ->
      K.katteyomi ("Coudn't find instruction in lookup table: " <> T.pack (show sh) <> " " <> T.pack (show opr)) ""
    Just instruction -> K.do
      env <- K.ask
      K.when env.assembler $ K.do
        K.katteyomi "" (T.toLower . T.pack $ show sh <> " " <> opr ^. oprIso <> "\n")
        assembleROMInternal instruction.opcode
      K.when env.emulator $ emulate
      K.when env.display $ K.do
        advanceTV instruction

-- TODO: We can do better than this double Maybe nonsense.
-- Same with filtering the validInstructions list TWICE.
instructText :: Shorthand -> T.Text -> Korigatachi () -> Korigatachi ()
instructText sh oprText emulate = 
  let
    relevantAddressingModes = InsOrd.toList $ (\ins -> ins.addressingMode) <$> K.filterShorthand sh
    parser = foldl1' (<|>) $ addressingModeToParser . snd <$> relevantAddressingModes
    maybeOpr = Attoparsec.maybeResult $ Attoparsec.parse parser oprText
    parsedAddressingMode = maybe "" toAddressingMode maybeOpr
    bytecode = fromMaybe 255 $ fst <$> List.find (\(_, x) -> parsedAddressingMode == x) relevantAddressingModes
    instruction = InsOrd.lookupDefault K.jamInstruction bytecode K.validInstructions
  in
    K.do
      env <- K.ask
      K.when env.assembler $ K.do
        K.katteyomi "" (T.toLower . T.pack $ show sh <> " " <> opr ^. oprIso <> "\n")
        assembleROMInternal instruction.opcode
      K.when env.emulator $ emulate
      K.when env.display $ K.do
        advanceTV instruction
    

dex :: Korigatachi ()
dex = instruct DEX Implied (K.ixpure ()) -- TODO: Decrement X by one.

dey :: Korigatachi ()
dey = instruct DEX Implied (K.ixpure ()) -- TODO: Decrement Y by one.

bne :: Operand -> Korigatachi ()
bne opr = instruct BNE opr (K.ixpure ()) -- TODO: Break if non zero.

jmp :: Operand -> Korigatachi ()
jmp opr = instruct JMP opr (K.ixpure ()) -- TODO: Jump to instruction.

cld :: Korigatachi ()
cld = instruct CLD Implied (clearFlags 0b000010000)

lda :: Operand -> Korigatachi ()
lda opr = instruct LDA opr $ K.do
  val <- readOpr opr
  K.modify $ #cpu % #generalRegisters % #a .~ val

ldx :: Operand -> Korigatachi ()
ldx opr = instruct LDX opr $ K.do
  val <- readOpr opr
  K.modify $ #cpu % #generalRegisters % #x .~ val

ldy :: Operand -> Korigatachi ()
ldy opr = instruct LDY opr $ K.do
  val <- readOpr opr
  K.modify $ #cpu % #generalRegisters % #y .~ val

sei :: Korigatachi ()
sei = instruct SEI Implied (setFlags 0b00000100)

sta :: Operand -> Korigatachi ()
sta opr = instruct STA opr $
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.a opr

stx :: Operand -> Korigatachi ()
stx opr = instruct STX opr $
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.x opr

sty :: Operand -> Korigatachi ()
sty opr = instruct STX opr $
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.y opr

txs :: Korigatachi ()
txs = instruct TXS Implied $
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

-- TODO: Unify "Operand" and "AddressingMode".
-- Something where we don't have to do this lookup every time.
lookupInstruction :: K.Shorthand -> Operand -> Maybe K.Instruction
lookupInstruction sh opr =
  let
    hashMap =
      InsOrd.elems $
        InsOrd.filter
          ( \instruction ->
              (instruction.shorthand == sh)
                && (instruction.addressingMode == toAddressingMode opr)
          )
          K.validInstructions
  in
    hashMap !? 0
