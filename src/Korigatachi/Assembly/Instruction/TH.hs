{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Korigatachi.Assembly.Instruction.TH where

import Data.Function ((&))
import Data.List (intersect, nub, sort, sortBy)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
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

instructions :: Q [Dec]
instructions = concat <$> traverse instructionSplice (Map.toList K.allInstructions)

ram :: QuasiQuoter
ram =
  QuasiQuoter
    { quoteExp = const $ fail "no"
    , quotePat = const $ fail "nuh-uh"
    , quoteType = const $ fail "nada"
    , quoteDec = ramDecs
    }

ramDecs :: String -> Q [Dec]
ramDecs str = undefined -- Issue 6. A quasiquoter for local variables.

{- | Construct an ASM mnemonic from a shorthand and a list of valid instructions.
This is an internal function, and should not be called by end users.
-}
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
          & sort
          & nub
      hasArity = any addressingModeArity addressingModes
      byteRelevantAddressingModes = [K.AMImmediate, K.AMRelative, K.AMZeroPage]
      wordRelevantAddressingModes = [K.AMAbsolute, K.AMIndirect]
  case hasArity of
    False -> do
      addModFinalizer $ putDoc (DeclDoc fnName) docString
      sequence
        [ sigD fnName [t|K.Assembly ()|]
        , valD (varP fnName) (normalB [|K.append $ K.Instruct $shE K.Implied|]) []
        ]
    True -> do
      addModFinalizer $ putDoc (DeclDoc fnName) docString
      -- \^ `addModFinalizer` allows you to reference function names
      -- after type-checking.
      [b, w, l, i, e] <- traverse newName ["b", "w", "l", "i", "e"]
      let byteBody x = [|K.append $ K.Instruct $shE $(appsE [conE $ mkName ("K." <> show x), varE b])|]
          wordBody x =
            [|
              let (ll, hh) = K.splitWord16 $(varE w) in K.append $ K.Instruct $shE ($(conE (mkName ("K." <> show x))) ll hh)
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
        , -- asm :: K.ASMArg a -> K.Assembly ()
          funD
            fnName
            [ byteClause
            , wordClause
            , clause [conP 'K.AALong [varP l]] (normalB longBody) []
            , clause [conP 'K.AAInt [varP i]] (normalB intBody) []
            , clause [conP 'K.AAExpr [varP e]] (normalB exprBody) []
            ]
        ]

addressingModeArity :: K.AddressingMode -> Bool
addressingModeArity = \case
  K.AMImmediate -> True
  K.AMIndirectX -> True
  K.AMIndirectY -> True
  K.AMRelative -> True
  K.AMZeroPage -> True
  K.AMZeroPageX -> True
  K.AMZeroPageY -> True
  K.AMAbsolute -> True
  K.AMAbsoluteX -> True
  K.AMAbsoluteY -> True
  K.AMIndirect -> True
  K.AMUnresolved -> True
  _ -> False
