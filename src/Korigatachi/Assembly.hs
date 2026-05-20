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

-- TODO: This is so fucking ugly.
instructText :: Shorthand -> T.Text -> (Operand -> Korigatachi ()) -> Korigatachi ()
instructText sh oprText emulate =
  let
    instructionTextRep = K.shorthandText sh <> " " <> oprText

    relevantAddressingModes = InsOrd.toList $ (\ins -> ins.addressingMode) <$> K.filterShorthand sh
    parser = foldl1' (<|>) $ addressingModeToParser . snd <$> relevantAddressingModes

    maybeOpr = Attoparsec.maybeResult $ Attoparsec.parse parser oprText
    opr = fromMaybe (Label oprText) maybeOpr
    parsedAddressingMode = maybe "" toAddressingMode maybeOpr
    bytecode = fromMaybe 255 $ fst <$> List.find (\(_, x) -> parsedAddressingMode == x) relevantAddressingModes
    instruction = InsOrd.lookupDefault K.jamInstruction bytecode K.validInstructions
  in
    K.do
      env <- K.ask
      K.when env.assembler $ K.do
        K.katteyomi "" instructionTextRep
        assembleROMInternal instruction.opcode
      K.when env.emulator $ (emulate opr)
      K.when env.display $ K.do
        advanceTV instruction

dex :: Korigatachi ()
dex = instructText DEX "Implied" (\_ -> K.ixpure ()) -- TODO: Decrement X by one.

dey :: Korigatachi ()
dey = instructText DEX "Implied" (\_ -> K.ixpure ()) -- TODO: Decrement Y by one.

bne :: T.Text -> Korigatachi ()
bne oprText = instructText BNE oprText (\_ -> K.ixpure ()) -- TODO: Break if non zero.

jmp :: T.Text -> Korigatachi ()
jmp oprText = instructText JMP oprText (\_ -> K.ixpure ()) -- TODO: Jump to instruction.

cld :: Korigatachi ()
cld = instructText CLD "Implied" (\_ -> clearFlags 0b000010000)

lda :: T.Text -> Korigatachi ()
lda oprText = instructText LDA oprText $ \opr -> K.do
  val <- readOpr opr
  K.modify $ #cpu % #generalRegisters % #a .~ val

ldx :: T.Text -> Korigatachi ()
ldx oprText = instructText LDX oprText $ \opr -> K.do
  val <- readOpr opr
  K.modify $ #cpu % #generalRegisters % #x .~ val

ldy :: T.Text -> Korigatachi ()
ldy oprText = instructText LDY oprText $ \opr -> K.do
  val <- readOpr opr
  K.modify $ #cpu % #generalRegisters % #y .~ val

sei :: Korigatachi ()
sei = instructText SEI "Implied" (\_ -> setFlags 0b00000100)

sta :: T.Text -> Korigatachi ()
sta oprText = instructText STA oprText $ \opr ->
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.a opr

stx :: T.Text -> Korigatachi ()
stx oprText = instructText STX oprText $ \opr ->
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.x opr

sty :: T.Text -> Korigatachi ()
sty oprText = instructText STX oprText $ \opr ->
  K.do
    atari <- K.get
    write atari.cpu.generalRegisters.y opr

txs :: Korigatachi ()
txs = instructText TXS "Implied" $ \_ ->
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
