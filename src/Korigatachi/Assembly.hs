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
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Use $>" -}

module Korigatachi.Assembly where

import Control.Applicative
import Data.Bits (Bits ((.&.)))
import Data.Text qualified as T
import Data.Word (Word16)
import Korigatachi.Assembly.Operand
import Korigatachi.Assembly.ReadWrite
import Korigatachi.Control qualified as K
import Korigatachi.Model qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types (Korigatachi)
import Korigatachi.Types qualified as K
import Optics
import Prelude hiding (and, read)

{- | Assembly instructions are indented to semantically
seperate them from labels and other not-assembly things.
This is hard-coded to two spaces because it looks nice.
-}
spacing :: T.Text
spacing = "  "

{- | The start of a valid Atari 2600 asm file.
Korigatachi is meant to not only assemble Atari machine code
itself, but also spit out assembly that is readable by the dasm
8-bit assembler.
-}
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

resolveLabel :: T.Text -> Korigatachi T.Text
resolveLabel labelText = K.do
  atari <- K.get
  let
    labels = atari.rom.labels
    shortCircuitFilter _ _ [] = 0
    shortCircuitFilter f predicate (x : xs) =
      if predicate x then f x else shortCircuitFilter f predicate xs
  pure . T.show $ shortCircuitFilter K.labelByte (\(K.MemoryLabel memoryLabel _) -> memoryLabel == labelText) labels

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
  assembleROM ll
  assembleROM hh
  K.codeGen (spacing <> ".word $" <> (T.pack $ w16 ^. K.hex16))

-- addressingModeArity :: T.Text -> Word8
-- addressingModeArity = \case
--   "Accumulator" -> 0
--   "Implied" -> 0
--   "Immediate" -> 1
--   "IndirectX" -> 1
--   "IndirectY" -> 1
--   "Relative" -> 1
--   "ZeroPage" -> 1
--   "ZeroPageX" -> 1
--   "ZeroPageY" -> 1
--   "Absolute" -> 2
--   "AbsoluteX" -> 2
--   "AbsoluteY" -> 2
--   "Indirect" -> 2
--   "Label" -> 1
--   _ -> 0

-- renderInstruction :: (K.Shorthand, [K.Instruction]) -> String
-- renderInstruction (sh, _insList) =
--   let
--     shtx = K.shorthandText sh
--     -- opcodeHex o = T.pack (printf "0x%02x" o)
--     -- firstOpcode = fromMaybe 0x02 (K.opcode <$> listToMaybe insList)
--     -- addressingModes = K.addressingMode <$> insList
--     -- arities = addressingModeArity <$> addressingModes
--     -- parseFnName = "parse" <> (T.toUpper shtx)
--     -- parserAlternatives = foldMap (\addrMode -> "parse" <> addrMode <> " <|> ") addressingModes <> "parseLabel"
--     -- buildRightCase (arity, instr) =
--     --   case arity of
--     --     0 ->
--     --       "Right " <> (K.addressingMode instr) <> "-> assembleROM " <> (opcodeHex $ K.opcode instr)
--     --     1 ->
--     --       "Right (" <> (K.addressingMode instr) <> " w8) -> void $ traverse assembleROM [" <> (opcodeHex $ K.opcode instr) <> ", w8]"
--     --     2 ->
--     --       "Right (" <> (K.addressingMode instr) <> " ll hh) -> void $ traverse assembleROM [" <> (opcodeHex $ K.opcode instr) <> ", ll, hh]"
--     --     _ -> error "Achievement Unlocked: Arity Rarity -- Build a case for an addressing mode with an arity greater than 2 or less than 0."
--     -- rightCases = ("    " <>) . buildRightCase <$> (zip arities insList)
--   in
--     -- case sum arities of
--     --   0 -> 
--         T.unpack . T.unlines $
--           [ shtx <> " :: Korigatachi ()"
--           , shtx <> " = undefined"
--           ]
--       -- 0 ->
--       --   T.unpack . T.unlines $
--       --     [ shtx <> " :: Korigatachi ()"
--       --     , shtx <> " = K.do"
--       --     , "  " <> "_ <- K.codeGen (spacing <> \"" <> shtx <> "\")"
--       --     , "  " <> "assembleROM " <> opcodeHex firstOpcode
--       --     ]
--       -- _ ->
--       --   T.unpack . T.unlines $
--       --     [ shtx <> " :: T.Text -> Korigatachi ()"
--       --     , shtx <> " oprText = K.do"
--       --     , "  _ <- K.codeGen (spacing <> \"" <> shtx <> " \" <> oprText)"
--       --     , "  let " <> parseFnName <> " = " <> parserAlternatives
--       --     , "  case Attoparsec.parseOnly " <> parseFnName <> " oprText of"
--       --     , "    Left _ -> K.log K.Warn (\"Failed to parse operand: \" <> oprText)"
--       --     ]
--       --       ++ rightCases
--       --       ++ [ "    Right (Label text) -> K.do"
--       --          , "      res <- resolveLabel text"
--       --          , "      " <> shtx <> " res"
--       --          , "    _ -> K.log K.Warn (\"Invalid operand for " <> T.toUpper shtx <> ": \" <> oprText)"
--       --          ]
-- renderInstruction' :: (K.Shorthand, [K.Instruction]) -> [T.Text]
-- renderInstruction' (sh, insList) =
--   let
--     shtx = K.shorthandText sh
--     opcodeHex o = T.pack (printf "0x%02x" o)
--     firstOpcode = fromMaybe 0x02 (K.opcode <$> listToMaybe insList)
--     addressingModes = K.addressingMode <$> insList
--     arities = addressingModeArity <$> addressingModes
--     parseFnName = "parse" <> (T.toUpper shtx)
--     parserAlternatives = foldMap (\addrMode -> "parse" <> addrMode <> " <|> ") addressingModes <> "parseLabel"
--     buildRightCase (arity, instr) =
--       case arity of
--         0 ->
--           "Right " <> (K.addressingMode instr) <> "-> assembleROM " <> (opcodeHex $ K.opcode instr)
--         1 ->
--           "Right (" <> (K.addressingMode instr) <> " w8) -> void $ traverse assembleROM [" <> (opcodeHex $ K.opcode instr) <> ", w8]"
--         2 ->
--           "Right (" <> (K.addressingMode instr) <> " ll hh) -> void $ traverse assembleROM [" <> (opcodeHex $ K.opcode instr) <> ", ll, hh]"
--         _ -> error "Achievement Unlocked: Arity Rarity -- Build a case for an addressing mode with an arity greater than 2 or less than 0."
--     rightCases = ("    " <>) . buildRightCase <$> (zip arities insList)
--   in
--     case sum arities of
--       0 ->
--           [ shtx <> " :: Korigatachi ()"
--           , shtx <> " = K.do"
--           , "  " <> "_ <- K.codeGen (spacing <> \"" <> shtx <> "\")"
--           , "  " <> "assembleROM " <> opcodeHex firstOpcode
--           ]
--       _ ->
        
--           [ shtx <> " :: T.Text -> Korigatachi ()"
--           , shtx <> " oprText = K.do"
--           , "  _ <- K.codeGen (spacing <> \"" <> shtx <> " \" <> oprText)"
--           , "  let " <> parseFnName <> " = " <> parserAlternatives
--           , "  case Attoparsec.parseOnly " <> parseFnName <> " oprText of"
--           , "    Left _ -> K.log K.Warn (\"Failed to parse operand: \" <> oprText)"
--           ]
--             ++ rightCases
--             ++ [ "    Right (Label text) -> K.do"
--                , "      res <- resolveLabel text"
--                , "      " <> shtx <> " res"
--                , "    _ -> K.log K.Warn (\"Invalid operand for " <> T.toUpper shtx <> ": \" <> oprText)"
--                ]

-- rendered' = T.unlines $ concatMap renderInstruction' (Map.toList K.allInstructions)

-- generateInstructions :: Q [Dec]
-- generateInstructions =
--   let
--     source = concatMap renderInstruction (Map.toList K.allInstructions)
--   in
--     case Meta.parseDecsWithMode korigatachiParseMode source of
--       Left err -> error $ "Achievement Unlocked: Get [Dec]ked -- Fail to generate functions using parseDecs. " <> err
--       Right decs -> pure decs

-- korigatachiParseMode :: Exts.ParseMode
-- korigatachiParseMode =
--   Exts.defaultParseMode
--     { Exts.extensions = Exts.UnknownExtension "QualifiedDo" : Exts.extensions Exts.defaultParseMode
--     }
--
advanceTV :: K.Instruction -> Korigatachi ()
advanceTV ins =
  K.modify (\atari -> atari {K.tv = K.advanceTV ins.cycles atari.tv})

-- Weird trick: change the transition state and then just put it back later.
-- K.do
--   atari <- K.get
--   K.modify (const ())
--   K.modify (const atari)
