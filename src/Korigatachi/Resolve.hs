{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Use $>" -}

module Korigatachi.Resolve where

import Data.Sequence qualified as Seq

-- import Data.Text qualified as T
-- import Data.Word (Word16)
-- import Korigatachi.Control qualified as K

import Data.Foldable (traverse_)
import Data.Text qualified as T
import Data.Word (Word16)
import Korigatachi.Assembly.Operand (splitWord16)
import Korigatachi.Control qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K
import Optics ((^.))
import Prelude hiding (and, read)

resolve :: K.Hane K.Assemble K.Resolve ()
resolve = K.do
  (K.Assemble statements) <- K.get
  -- Neat trick!
  K.put $ 
    K.Resolve
      { K.resolveStatements = statements
      , K.resolveLabels = Seq.empty
      , K.resolveCodegen = Seq.empty
      , K.resolveProgramCounter = 0
      }
  let

    indexedStatements = Seq.zip (Seq.fromList [(0 :: Int) ..65535]) statements

    resolveCodegenHane :: K.Statement -> K.Hane K.Resolve K.Resolve ()
    resolveCodegenHane statement = K.do
      K.modify $ \rsv@(K.Resolve {..}) -> rsv {K.resolveCodegen = resolveCodegen Seq.|> renderStatement statement}

    resolveTopLevelLabelHane :: K.Statement -> K.Hane K.Resolve K.Resolve ()
    resolveTopLevelLabelHane statement = K.do
      case statement of
        K.Org w16 -> K.modify $ \rsv -> rsv {K.resolveProgramCounter = w16}
        K.TopLevelLabel label -> K.modify $ \rsv@(K.Resolve {..}) -> rsv {K.resolveLabels = resolveLabels Seq.|> (resolveProgramCounter, label)}
        K.Instruct _ opr -> K.modify $ \rsv@(K.Resolve {..}) -> rsv {K.resolveProgramCounter = resolveProgramCounter + operandToProgramCount opr}
        K.Word _ -> K.modify $ \rsv@(K.Resolve {..}) -> rsv {K.resolveProgramCounter = resolveProgramCounter + 2}
        _ -> pure ()

    resolveStatementsHane :: Int -> K.Statement -> K.Hane K.Resolve K.Resolve ()
    resolveStatementsHane statementIndex statement = K.do
      case statement of
        K.Instruct sh opr ->
          case opr of
            K.Label labelAddrModes lb -> K.do
              labels <- K.resolveLabels <$> K.get
              res <- resolveLabel labels labelAddrModes lb
              K.modify $ \rsv@(K.Resolve {..}) -> rsv {K.resolveStatements = Seq.update statementIndex (K.Instruct sh res) resolveStatements}
            _ -> pure ()
        _ -> pure ()

  traverse_ (resolveCodegenHane >> resolveTopLevelLabelHane) statements -- Technically, this is Pass 2.
  traverse_ (uncurry resolveStatementsHane) indexedStatements -- Technically, this is Pass 4.
  pure ()

-- | Refers to labels in the past AND future!
resolveLabel :: Seq.Seq (Word16, T.Text) -> [K.LabelAddressing] -> T.Text -> K.Hane K.Resolve K.Resolve K.Operand
resolveLabel labels labelAddressing toResolve = K.do
  pc <- K.resolveProgramCounter <$> K.get
  let
    found = Seq.lookup 0 $ Seq.filter (\(_, lb) -> lb == toResolve) labels
  case (found, labelAddressing) of
    (Nothing, _) -> K.do
      K.log K.Warn $ "Unrecognized label: " <> toResolve
      pure $ K.Label [] ""
    (_, []) -> K.do
      K.log K.Warn "Missing label addressing modes."
      pure $ K.Label [] ""
    (Just (addr, _), (labelAddrMode : _)) ->
      pure $ reifyLabel labelAddrMode pc addr

-- | TODO: Better name!
reifyLabel :: K.LabelAddressing -> Word16 -> Word16 -> K.Operand
reifyLabel la pc addr =
  let
    (hh, ll) = splitWord16 addr
    intAddr :: Int
    intAddr = fromIntegral addr
    intPC :: Int
    intPC = fromIntegral pc
    diff = fromIntegral $ intPC - intAddr -- This works cause of literal overflow.
  in
    case la of
      K.LabelAbsolute -> K.Absolute hh ll
      K.LabelRelative -> K.Relative diff
      K.LabelIndirect -> K.Indirect hh ll

renderStatement :: K.Statement -> T.Text
renderStatement = \case
  K.Org w16 -> "  org $" <> (T.pack $ w16 ^. K.hex16)
  K.Word w16 -> "  .word $" <> (T.pack $ w16 ^. K.hex16)
  K.Processor processor -> "  processor" <> processor
  K.Include include -> "  include" <> include
  K.TopLevelLabel label -> label
  K.Instruct short opr -> "  " <> (T.toLower $ T.show short) <> " " <> (T.pack $ operandToString opr)

operandToString :: K.Operand -> String
operandToString K.Accumulator = "A" -- For completeness.
operandToString K.Implied = "" -- For completeness.
operandToString (K.Immediate opr) = "#$" <> (opr ^. K.hex8)
operandToString (K.IndirectX opr) = "($" <> (opr ^. K.hex8) <> ",x)"
operandToString (K.IndirectY opr) = "($" <> (opr ^. K.hex8) <> "),y"
operandToString (K.Relative opr) = "r$" <> (opr ^. K.hex8)
operandToString (K.ZeroPage opr) = "$" <> (opr ^. K.hex8)
operandToString (K.ZeroPageX opr) = "$" <> (opr ^. K.hex8) <> ",x"
operandToString (K.ZeroPageY opr) = "$" <> (opr ^. K.hex8) <> ",y"
operandToString (K.Absolute ll hh) =
  let
    low :: Word16
    low = fromIntegral ll
    high :: Word16
    high = fromIntegral hh
  in
    "$" <> ((high * 256 + low) ^. K.hex16) -- order of operations?
operandToString (K.AbsoluteX ll hh) =
  let
    low :: Word16
    low = fromIntegral ll
    high :: Word16
    high = fromIntegral hh
  in
    "$" <> ((high * 256 + low) ^. K.hex16) <> ",x" -- order of operations?
operandToString (K.AbsoluteY ll hh) =
  let
    low :: Word16
    low = fromIntegral ll
    high :: Word16
    high = fromIntegral hh
  in
    "$" <> ((high * 256 + low) ^. K.hex16) <> ",y" -- order of operations?
operandToString (K.Indirect ll hh) =
  let
    low :: Word16
    low = fromIntegral ll
    high :: Word16
    high = fromIntegral hh
  in
    "($" <> ((high * 256 + low) ^. K.hex16) <> ")" -- order of operations?
operandToString (K.Label _ lb) = T.unpack lb

operandToProgramCount :: K.Operand -> Word16
operandToProgramCount K.Accumulator = 1
operandToProgramCount K.Implied = 1
operandToProgramCount (K.Immediate _) = 2
operandToProgramCount (K.IndirectX _) = 2
operandToProgramCount (K.IndirectY _) = 2
operandToProgramCount (K.Relative _) = 2
operandToProgramCount (K.ZeroPage _) = 2
operandToProgramCount (K.ZeroPageX _) = 2
operandToProgramCount (K.ZeroPageY _) = 2
operandToProgramCount (K.Absolute _ _) = 3
operandToProgramCount (K.AbsoluteX _ _) = 3
operandToProgramCount (K.AbsoluteY _ _) = 3
operandToProgramCount (K.Indirect _ _) = 3
operandToProgramCount (K.Label labelAddrModes _) = foldl1 max $ labelAddressingModeProgramCount <$> labelAddrModes

-- | This is fine actually, since Relative is exclusive to conditional branch instructions.
labelAddressingModeProgramCount :: K.LabelAddressing -> Word16
labelAddressingModeProgramCount = \case
  K.LabelIndirect -> 2
  K.LabelRelative -> 1
  K.LabelAbsolute -> 2

-- Indirect & Absolute can only be confused for each other with JMP ($4C and $6C).
-- Disambiguating between the two would require a special syntax for "Indirect Labels".
-- Something like "jmp (Start)". Which might be cool. And possibly useful.
-- But not particularly fun to implement. Maybe something could be done with the TH implementation of JMP?
-- Failed parses of Indirect that start with '(' get parsed down to Label [LabelIndirect] (tx :: T.Text).
-- It'd require breaking apart the TH parseDecs call so much though. Just for this one case.
