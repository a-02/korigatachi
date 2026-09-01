{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Korigatachi.Assembly.Instruction.TH where

import Data.List (intersect)
import Data.Map qualified as Map
import Data.Text qualified as T
import Korigatachi.Atari.Model qualified as K
import Korigatachi.Types qualified as K
import Language.Haskell.Meta.Parse qualified as Meta
import Language.Haskell.TH

generateInstructions :: Q [Dec]
generateInstructions =
  let
    source = T.unpack . T.unlines $ concatMap renderInstruction (Map.toList K.allInstructions)
  in
    case Meta.parseDecs source of
      Left err ->
        error $ "Achievement Unlocked: Get [Dec]ked -- Fail to generate functions using parseDecs. " <> err
      Right decs -> pure decs

addressingModeArity :: T.Text -> Bool
addressingModeArity = \case
  "Accumulator" -> False
  "Implied" -> False
  "Immediate" -> True
  "IndirectX" -> True
  "IndirectY" -> True
  "Relative" -> True
  "ZeroPage" -> True
  "ZeroPageX" -> True
  "ZeroPageY" -> True
  "Absolute" -> True
  "AbsoluteX" -> True
  "AbsoluteY" -> True
  "Indirect" -> True
  "Label" -> True
  _ -> False

renderInstruction :: (K.Shorthand, [K.Instruction]) -> [T.Text]
renderInstruction (sh, insList) =
  let
    lowercased = T.toLower $ T.show sh
    addressingModes = K.addressingMode <$> insList
    labelAddressingModes =
      (\x -> "[" <> x <> "]") . T.intercalate "," $
        ("K.Label" <>) <$> addressingModes `intersect` ["Relative", "Absolute", "Indirect"]
    hasArity = foldl1 (||) $ addressingModeArity <$> addressingModes
    parseFnName = "parse" <> (T.show sh)
    parserAlternatives = foldMap (\addrMode -> "parse" <> addrMode <> " <|> ") addressingModes <> "parseLabel"
  in
    if hasArity
      then
        [ lowercased <> " :: T.Text -> K.Assembly ()"
        , lowercased <> " oprText ="
        , "  let " <> parseFnName <> " = " <> parserAlternatives
        , "   in case Attoparsec.parseOnly " <> parseFnName <> " oprText of"
        , "        Left _ -> K.log K.Warn (\"Failed to parse operand: \" <> oprText)"
        , "        Right (K.Label _ lb) ->"
        , "          K.append $ K.Instruct K." <> (T.show sh) <> " (K.Label " <> labelAddressingModes <> " lb)"
        , "        Right parsedOpr -> K.append $ K.Instruct K." <> (T.show sh) <> " parsedOpr"
        ]
      else
        [ lowercased <> " :: K.Assembly ()"
        , lowercased <> " = K.append $ K.Instruct K." <> (T.show sh) <> " K.Implied"
        ]
