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

module Korigatachi.Assembly.CodeGen where

-- Generating Haskell source, not assembly. What'd you think it was?

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Word (Word8)
import Korigatachi.Model qualified as K
import Korigatachi.Types qualified as K
import Text.Printf (printf)

addressingModeArity :: T.Text -> Word8
addressingModeArity = \case
  "Accumulator" -> 0
  "Implied" -> 0
  "Immediate" -> 1
  "IndirectX" -> 1
  "IndirectY" -> 1
  "Relative" -> 1
  "ZeroPage" -> 1
  "ZeroPageX" -> 1
  "ZeroPageY" -> 1
  "Absolute" -> 2
  "AbsoluteX" -> 2
  "AbsoluteY" -> 2
  "Indirect" -> 2
  "Label" -> 1
  _ -> 0

renderInstruction :: (K.Shorthand, [K.Instruction]) -> [T.Text]
renderInstruction (sh, insList) =
  let
    shtx = K.shorthandText sh
    opcodeHex o = T.pack (printf "0x%02x" o)
    firstOpcode = fromMaybe 0x02 (K.opcode <$> listToMaybe insList)
    addressingModes = K.addressingMode <$> insList
    arities = addressingModeArity <$> addressingModes
    parseFnName = "parse" <> (T.toUpper shtx)
    parserAlternatives = foldMap (\addrMode -> "parse" <> addrMode <> " <|> ") addressingModes <> "parseLabel"
    buildRightCase (arity, instr) =
      case arity of
        0 ->
          ("    " <>)
            <$> [ "Right " <> (K.addressingMode instr) <> "-> K.do"
                , "  assembleROM " <> (opcodeHex $ K.opcode instr)
                , "  " <> "K.codeGen (spacing <> \"" <> shtx <> "\")"
                ]
        1 ->
          ("    " <>)
            <$> [ "Right (" <> (K.addressingMode instr) <> " w8) -> K.do"
                , "  void $ traverse assembleROM [" <> (opcodeHex $ K.opcode instr) <> ", w8]"
                , "  K.codeGen (spacing <> \"" <> shtx <> " \" <> oprText)"
                ]
        2 ->
          ("    " <>)
            <$> [ "Right (" <> (K.addressingMode instr) <> " ll hh) -> K.do"
                , "  void $ traverse assembleROM [" <> (opcodeHex $ K.opcode instr) <> ", ll, hh]"
                , "  K.codeGen (spacing <> \"" <> shtx <> " \" <> oprText)"
                ]
        _ -> error "Achievement Unlocked: Arity Rarity -- Build a case for an addressing mode with an arity greater than 2 or less than 0."
    rightCases = foldMap buildRightCase (zip arities insList)
  in
    case sum arities of
      0 ->
        [ shtx <> " :: Korigatachi ()"
        , shtx <> " = K.do"
        , "  " <> "assembleROM " <> opcodeHex firstOpcode
        ]
      _ ->
        [ shtx <> " :: T.Text -> Korigatachi ()"
        , shtx <> " oprText = K.do"
        , "  let " <> parseFnName <> " = " <> parserAlternatives
        , "  case Attoparsec.parseOnly " <> parseFnName <> " oprText of"
        , "    Left _ -> K.log K.Warn (\"Failed to parse operand: \" <> oprText)"
        ]
          ++ rightCases
          ++ [ "    Right (Label text) -> K.do"
             , "      res <- resolveLabel text"
             , "      " <> shtx <> " res"
             , "    _ -> K.log K.Warn (\"Invalid operand for " <> T.toUpper shtx <> ": \" <> oprText)"
             ]

renderedSource :: T.Text
renderedSource = T.unlines $ concatMap renderInstruction (Map.toList K.allInstructions)

-- generateInstructions :: Q [Dec]
-- generateInstructions =
--   let
--     source = concatMap renderInstruction (Map.toList K.allInstructions)
--   in
--     case Meta.parseDecsWithMode korigatachiParseMode source of
--       Left err -> error $ "Achievement Unlocked: Get [Dec]ked -- Fail to generate functions using parseDecs. " <> err
--       Right decs -> pure decs
