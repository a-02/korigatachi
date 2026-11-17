{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Korigatachi.Assembly.Instruction.TH where

import Data.Function ((&))
import Data.List (intersect, nub, sortBy)
import Data.Map qualified as Map
import Data.Text qualified as T
import Korigatachi.Assembly.Control qualified as K
import Korigatachi.Atari.Model qualified as K
import Korigatachi.Atari.Types qualified as K
import Korigatachi.Control qualified as K
import Korigatachi.Types qualified as K
import Language.Haskell.Meta.Parse qualified as Meta
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

generateInstructions :: Q [Dec]
generateInstructions =
  let
    source = T.unpack . T.unlines $ concatMap renderInstruction (Map.toList K.allInstructions)
  in
    case Meta.parseDecs source of
      Left err ->
        error $ "Achievement Unlocked: [Dec]ked out -- Fail to generate functions using parseDecs. " <> err
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

addressingModeIs8Bit :: T.Text -> Bool
addressingModeIs8Bit = \case
  "Accumulator" -> True
  "Immediate" -> True
  "IndirectX" -> True
  "IndirectY" -> True
  "Relative" -> True
  "ZeroPage" -> True
  "ZeroPageX" -> True
  "ZeroPageY" -> True
  _ -> False

addressingModeIs16Bit :: T.Text -> Bool
addressingModeIs16Bit = \case
  "Absolute" -> True
  "AbsoluteX" -> True
  "AbsoluteY" -> True
  "Indirect" -> True
  _ -> False

addressingModePrecedence :: T.Text -> Int
addressingModePrecedence = \case
  "IndirectX" -> 1
  "IndirectY" -> 2
  "ZeroPageX" -> 3
  "ZeroPageY" -> 4
  "Relative" -> 5
  "ZeroPage" -> 6
  "Absolute" -> 7
  "AbsoluteX" -> 8
  "AbsoluteY" -> 9
  "Indirect" -> 10
  "Immediate" -> 11
  "Implied" -> 12
  "Accumulator" -> 13
  "Label" -> maxBound -- Redundant, but nice to have.
  _ -> maxBound

instructionSplice :: (K.Shorthand, [K.Instruction]) -> Q [Dec]
instructionSplice (sh, insList) = do
  let
    shE = lift sh
    fnName =
      (T.show sh)
        & T.toLower
        & T.unpack
        & mkName
    docString =
      (K.description <$> insList)
        & nub
        & T.unlines
        & T.unpack
    addressingModes =
      (K.addressingMode <$> insList)
        & sortBy comparePrecedence
        & nub
    hasArity = any addressingModeArity addressingModes
    byteRelevantAddressingModes = ["Immediate", "Relative", "ZeroPage"]
    wordRelevantAddressingModes = ["Absolute", "Indirect"]
  case hasArity of
    False -> do
      putDoc (DeclDoc fnName) docString
      sequence
        [ sigD fnName [t|K.Assembly ()|]
        , valD (varP fnName) (normalB [|K.append $ K.Instruct $shE K.Implied|]) []
        ]
    True -> do
      putDoc (DeclDoc fnName) docString
      [b, w, l, i, e] <- traverse newName ["b", "w", "l", "i", "e"]
      let
        byteBody = case addressingModes `intersect` byteRelevantAddressingModes of
          (x : _) -> [|K.append $ K.Instruct $shE $(appE (conE (mkName ("K." <> T.unpack x))) (varE b))|]
          [] -> [|pure ()|] -- TODO: Add error logic to miscalled cases.
        wordBody = case addressingModes `intersect` wordRelevantAddressingModes of
          (x : _) ->
            [|
              let
                (ll, hh) = splitWord16 $(varE w)
              in
                K.append $ K.Instruct $shE ($(conE (mkName ("K." <> T.unpack x))) ll hh)
              |]
          [] -> [|pure ()|] -- TODO: Add error logic to miscalled cases.
        longBody = [|K.log K.Warn "No longs yet."|]
        intBody = [|K.log K.Warn "No ints yet."|]
      sequence
        [ sigD fnName [t|K.ASMArg -> K.Assembly ()|]
        , funD
            fnName
            [ clause [conP 'K.AAByte [varP b]] (normalB byteBody) []
            , clause [conP 'K.AAWord [varP w]] (normalB wordBody) []
            , clause [conP 'K.AALong [varP l]] (normalB longBody) []
            , clause [conP 'K.AAInt [varP i]] (normalB intBody) []
            , clause [conP 'K.AAExpr [varP e]] (normalB undefined) []
            ]
        ]

renderInstruction :: (K.Shorthand, [K.Instruction]) -> [T.Text]
renderInstruction (sh, insList) =
  let
    lowercased = T.toLower $ T.show sh
    addressingModes = nub $ sortBy comparePrecedence $ K.addressingMode <$> insList
    labelAddressingModes =
      (\x -> "[" <> x <> "]") . T.intercalate "," $
        ("K.Label" <>) <$> addressingModes `intersect` ["Relative", "Absolute", "Indirect"]
    hasArity = foldl1 (||) $ addressingModeArity <$> addressingModes
    parseFnName = "parse" <> (T.show sh)
    parserAlternatives = foldMap (\addrMode -> "parse" <> addrMode <> " <|> ") addressingModes <> "parseLabel"
  in
    case sh of
      K.JMP ->
        [ "-- | Jump. "
        , "jmp :: T.Text -> K.Assembly ()"
        , "jmp oprText ="
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
        , "            K.append $ K.Instruct K." <> (T.show sh) <> " (K.Label [K.LabelIndirect] stripped)"
        , "          else"
        , "            K.append $ K.Instruct K." <> (T.show sh) <> " (K.Label [K.LabelAbsolute] lb)"
        , "        Right parsedOpr -> K.append $ K.Instruct K." <> (T.show sh) <> " parsedOpr"
        ]
      _ ->
        if hasArity
          then
            [ "-- | " <> (T.unlines . nub $ (("-- " <>) . K.description) <$> insList)
            , lowercased <> " :: T.Text -> K.Assembly ()"
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

comparePrecedence :: T.Text -> T.Text -> Ordering
comparePrecedence textA textB = (addressingModePrecedence textA) `compare` (addressingModePrecedence textB)
