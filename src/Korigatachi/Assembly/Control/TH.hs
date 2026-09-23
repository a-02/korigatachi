{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Korigatachi.Assembly.Control.TH where

import Data.List (intersect, nub, sortBy)
import Data.Map qualified as Map
import Data.Text qualified as T
import Korigatachi.Atari.Model qualified as K
import Korigatachi.Types qualified as K
import Language.Haskell.Meta.Parse qualified as Meta
import Language.Haskell.TH

generateInstructions :: Q [Dec]
generateInstructions =
  let
    source = T.unpack . T.unlines $ concatMap renderInstructions (Map.toList K.allInstructions)
  in
    case Meta.parseDecs source of
      Left err ->
        error $ "Achievement Unlocked: Get [Dec]ked -- Fail to generate functions using parseDecs. " <> err
      Right decs -> pure decs

generateInstances :: Q [Dec]
generateInstances =
  let
    source =
      T.unpack . T.unlines $
        ["instance Operand Word8 where"]
          <> concatMap (fmap ("  " <>) . renderInstructionsWord8) (Map.toList K.allInstructions)
          <> [ "  ins1 sh0 _ = ins0 sh0"
             , "instance Operand Word16 where"
             ]
          <> concatMap (fmap ("  " <>) . renderInstructionsWord16) (Map.toList K.allInstructions)
          <> [ "  ins1 sh0 _ = ins0 sh0"
             , "instance Operand T.Text where"
             ]
          <> concatMap (fmap ("  " <>) . renderInstructionsText) (Map.toList K.allInstructions)
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

addressingModePrecedence :: T.Text -> Int
addressingModePrecedence = \case
  "Accumulator" -> 13
  "Implied" -> 12
  "Immediate" -> 11
  "IndirectX" -> 1
  "IndirectY" -> 2
  "Relative" -> 5
  "ZeroPage" -> 6
  "ZeroPageX" -> 3
  "ZeroPageY" -> 4
  "Absolute" -> 7
  "AbsoluteX" -> 8
  "AbsoluteY" -> 9
  "Indirect" -> 10
  "Label" -> maxBound
  _ -> maxBound

renderInstructionsWord8 :: (K.Shorthand, [K.Instruction]) -> [T.Text]
renderInstructionsWord8 (short, insList) =
  let
    sh = T.show short
    addressingModes = nub $ sortBy comparePrecedence $ K.addressingMode <$> insList
    hasArity = foldl1 (||) $ addressingModeArity <$> addressingModes
  in
    case hasArity of
      True ->
        ["ins1 K." <> sh <> " w8 = append $ K.Instruct K." <> sh <> " (K.ZeroPage w8)"]
      False ->
        []

renderInstructionsWord16 :: (K.Shorthand, [K.Instruction]) -> [T.Text]
renderInstructionsWord16 (short, insList) =
  let
    sh = T.show short
    addressingModes = nub $ sortBy comparePrecedence $ K.addressingMode <$> insList
    hasArity = foldl1 (||) $ addressingModeArity <$> addressingModes
  in
    case hasArity of
      True ->
        ["ins1 K." <> sh <> " w16 = append $ K.Instruct K." <> sh <> " (uncurry K.Absolute (K.splitWord16 w16))"]
      False ->
        []

renderInstructionsText :: (K.Shorthand, [K.Instruction]) -> [T.Text]
renderInstructionsText (short, insList) =
  let
    sh = T.show short
    addressingModes = nub $ sortBy comparePrecedence $ K.addressingMode <$> insList
    labelAddressingModes =
      (\x -> "[" <> x <> "]") . T.intercalate "," $
        ("K.Label" <>) <$> addressingModes `intersect` ["Relative", "Absolute", "Indirect"]
    hasArity = foldl1 (||) $ addressingModeArity <$> addressingModes
    parseFnName = "parse" <> sh
    parserAlternatives = foldMap (\addrMode -> "parse" <> addrMode <> " <|> ") addressingModes <> "parseLabel"
  in
    case short of
      K.JMP ->
        [ "ins1 K.JMP oprText ="
        , "  let parseJMP = " <> parserAlternatives
        , -- Aha. Aaaaha.
          "      stripped = fromMaybe \"\" $ ((snd <$>) . T.uncons) >=> ((fst <$>) . T.unsnoc) $ oprText"
        , "      firstChar = snd <$> T.unsnoc oprText"
        , "      lastChar = fst <$> T.uncons oprText"
        , "   in case Attoparsec.parseOnly parseJMP oprText of"
        , "        Left _ -> K.log K.Warn (\"Failed to parse operand: \" <> oprText)"
        , "        Right (K.Label _ lb) ->"
        , "          if (firstChar == Just \'(\') && (lastChar == Just \')\')"
        , "          then"
        , "            append $ K.Instruct K." <> sh <> " (K.Label [K.LabelIndirect] stripped)"
        , "          else"
        , "            append $ K.Instruct K." <> sh <> " (K.Label [K.LabelAbsolute] lb)"
        , "        Right parsedOpr -> append $ K.Instruct K." <> sh <> " parsedOpr"
        ]
      _ ->
        if hasArity
          then
            [ "ins1 K." <> sh <> " oprText ="
            , "  let " <> parseFnName <> " = " <> parserAlternatives
            , "   in case Attoparsec.parseOnly " <> parseFnName <> " oprText of"
            , "        Left _ -> K.log K.Warn (\"Failed to parse operand: \" <> oprText)"
            , "        Right (K.Label _ lb) ->"
            , "          append $ K.Instruct K." <> sh <> " (K.Label " <> labelAddressingModes <> " lb)"
            , "        Right parsedOpr -> append $ K.Instruct K." <> sh <> " parsedOpr"
            ]
          else
            []

renderInstructions :: (K.Shorthand, [K.Instruction]) -> [T.Text]
renderInstructions (short, insList) =
  let
    sh = T.show short
    lowercased = T.toLower sh
    addressingModes = nub $ sortBy comparePrecedence $ K.addressingMode <$> insList
    hasArity = foldl1 (||) $ addressingModeArity <$> addressingModes
  in
    case hasArity of
      True ->
        [ lowercased <> " :: Operand o => o -> K.Assembly ()"
        , lowercased <> " o = ins1 K." <> sh <> " o"
        ]
      False ->
        [ lowercased <> " :: K.Assembly ()"
        , lowercased <> " = ins0 K." <> sh
        ]

comparePrecedence :: T.Text -> T.Text -> Ordering
comparePrecedence textA textB = (addressingModePrecedence textA) `compare` (addressingModePrecedence textB)
