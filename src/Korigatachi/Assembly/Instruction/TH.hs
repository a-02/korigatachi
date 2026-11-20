{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Korigatachi.Assembly.Instruction.TH where

import Data.Function ((&))
import Data.List (intersect, nub, sortBy)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addModFinalizer, lift)

import Data.Map qualified as Map
import Data.Text qualified as T
import Language.Haskell.Meta.Parse qualified as Meta

import Korigatachi.Assembly.Control qualified as K
import Korigatachi.Assembly.Operand qualified as K
import Korigatachi.Atari.Model qualified as K
import Korigatachi.Atari.Types qualified as K
import Korigatachi.Control qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K

generateInstructions :: Q [Dec]
generateInstructions =
  let source = T.unpack . T.unlines $ concatMap renderInstruction (Map.toList K.allInstructions)
   in case Meta.parseDecs source of
        Left err ->
          error $ "Achievement Unlocked: [Dec]ked out -- Fail to generate functions using parseDecs. " <> err
        Right decs -> pure decs

instructions :: Q [Dec]
instructions = concat <$> traverse instructionSplice (Map.toList K.allInstructions)

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
  let shE = lift sh
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
      addModFinalizer $ putDoc (DeclDoc fnName) docString
      sequence
        [ sigD fnName [t|K.Assembly ()|]
        , valD (varP fnName) (normalB [|K.append $ K.Instruct $shE K.Implied|]) []
        ]
    True -> do
      addModFinalizer $ putDoc (DeclDoc fnName) docString
      [b, w, l, i, e] <- traverse newName ["b", "w", "l", "i", "e"]
      let byteBody x = [|K.append $ K.Instruct $shE $(appsE [conE $ mkName ("K." <> T.unpack x), varE b])|]
          wordBody x =
            [|
              let (ll, hh) = K.splitWord16 $(varE w) in K.append $ K.Instruct $shE ($(conE (mkName ("K." <> T.unpack x))) ll hh)
              |]
          byteClause = case addressingModes `intersect` byteRelevantAddressingModes of
            (x : _) ->
              clause [conP 'K.AAByte [varP b]] (normalB (byteBody x)) []
            [] -> clause [conP 'K.AAByte [wildP]] (normalB [|pure ()|]) []
          wordClause = case addressingModes `intersect` wordRelevantAddressingModes of
            (x : _) ->
              clause [conP 'K.AAWord [varP w]] (normalB (wordBody x)) []
            [] -> clause [conP 'K.AAWord [wildP]] (normalB [|pure ()|]) []
          longBody = [|K.log K.Warn $ "Tried to call " <> (T.show $shE) <> " with LONG argument: " <> (T.show $(varE l))|]
          intBody = [|K.log K.Warn $ "Tried to call " <> (T.show $shE) <> " with INT argument: " <> (T.show $(varE i))|]
          exprBody =
            [|
              K.do
                K.log K.Warn "Experimental! Watch out!."
                K.append $ K.Instruct $shE (K.Unresolved $(varE e))
              |]
      sequence
        [ sigD fnName [t|K.ASMArg $(varT (mkName "a")) -> K.Assembly ()|]
        , funD
            fnName
            [ byteClause
            , wordClause
            , clause [conP 'K.AALong [varP l]] (normalB longBody) []
            , clause [conP 'K.AAInt [varP i]] (normalB intBody) []
            , clause [conP 'K.AAExpr [varP e]] (normalB exprBody) []
            ]
        ]

renderInstruction :: (K.Shorthand, [K.Instruction]) -> [T.Text]
renderInstruction (sh, insList) =
  let lowercased = T.toLower $ T.show sh
      addressingModes = nub $ sortBy comparePrecedence $ K.addressingMode <$> insList
      labelAddressingModes =
        (\x -> "[" <> x <> "]") . T.intercalate "," $
          ("K.Label" <>) <$> addressingModes `intersect` ["Relative", "Absolute", "Indirect"]
      hasArity = foldl1 (||) $ addressingModeArity <$> addressingModes
      parseFnName = "parse" <> (T.show sh)
      parserAlternatives = foldMap (\addrMode -> "parse" <> addrMode <> " <|> ") addressingModes <> "parseLabel"
   in case sh of
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
