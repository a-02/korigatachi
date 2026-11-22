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

import Korigatachi.Assembly.Control qualified as K
import Korigatachi.Assembly.Operand qualified as K
import Korigatachi.Atari.Model qualified as K
import Korigatachi.Atari.Types qualified as K
import Korigatachi.Control qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K
import Language.Haskell.TH.Quote

instructions :: Q [Dec]
instructions = concat <$> traverse instructionSplice (Map.toList K.allInstructions)

ram :: QuasiQuoter
ram = QuasiQuoter
  { quoteExp = const $ fail "no"
  , quotePat = const $ fail "nuh-uh"
  , quoteType = const $ fail "nada"
  , quoteDec = ramDecs
  }

ramDecs :: String -> Q [Dec]
ramDecs str = undefined -- Issue 6. A quasiquoter for local variables.

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

comparePrecedence :: T.Text -> T.Text -> Ordering

comparePrecedence textA textB = (addressingModePrecedence textA) `compare` (addressingModePrecedence textB)
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
