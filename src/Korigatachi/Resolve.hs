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

import Data.Functor (void)
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
  K.put $ -- Initialize the Resolve state.
    K.Resolve
      { K.resolveStatements = Seq.empty
      , K.resolveLabels = Seq.empty
      , K.resolveCodegen = Seq.empty
      , K.resolveProgramCounter = 0
      }
  let
    resolveStatement :: K.Statement -> K.Hane K.Resolve K.Resolve ()
    resolveStatement statement = K.do
      -- These don't change per statement.
      K.modify $
        \rsv@(K.Resolve {..}) ->
          rsv
            { K.resolveStatements = resolveStatements Seq.|> statement
            , K.resolveCodegen = resolveCodegen Seq.|> renderStatement statement
            }
      -- These do.
      case statement of
        K.TopLevelLabel label -> K.modify $ \rsv@(K.Resolve {..}) -> rsv {K.resolveLabels = resolveLabels Seq.|> (resolveProgramCounter, label)}
        K.Org w16 -> K.modify $ \rsv -> rsv {K.resolveProgramCounter = w16}
        K.Instruct sh opr ->
          case opr of
            K.Label labelAddrModes lb -> K.do
              labels <- K.resolveLabels <$> K.get
              res <- resolveLabel labels labelAddrModes lb
              K.modify $ \rsv@(K.Resolve {..}) ->
                rsv
                  { K.resolveProgramCounter = resolveProgramCounter + operandToProgramCount opr
                  , K.resolveStatements =
                      resolveStatements
                        Seq.|> (K.Instruct sh res)
                  }
            _ -> K.modify $ \rsv@(K.Resolve {..}) -> rsv {K.resolveProgramCounter = resolveProgramCounter + operandToProgramCount opr}
        _ -> pure ()
  void $ traverse resolveStatement statements
  pure ()

-- There's some opportunity for improvement here. This being an eDSL and not a traditional
-- assembler, there's no way for a Zero Page label to exist. Defining a local variable in
-- the "assembly" would be done using as Haskell let binding and doesn't need to be
-- represented in the abstract here.
--
-- We need to implement the special Relative Addressing behavior. Well. I do.

resolveLabel :: Seq.Seq (Word16, T.Text) -> [K.LabelAddressing] -> T.Text -> K.Hane K.Resolve K.Resolve K.Operand
resolveLabel labels labelAddressing toResolve =
  let
    found = Seq.lookup 0 $ Seq.filter (\(_, lb) -> lb == toResolve) labels
  in
    case (found, labelAddressing) of
      (Nothing, _) -> K.do
        K.log K.Warn $ "Unrecognized label: " <> toResolve
        pure $ K.Label [] ""
      (_, []) -> K.do
        K.log K.Warn "Missing label addressing modes."
        pure $ K.Label [] "Missing label addressing modes. Check the logs."
      (Just (addr, _), (a : _)) ->
        -- go away
        pure $ reifyLabel a addr

-- | TODO: Better name!
reifyLabel :: K.LabelAddressing -> Word16 -> K.Operand
reifyLabel la w16 =
  let
    (hh, ll) = splitWord16 w16
  in
    case la of
      K.LabelAbsolute -> K.Absolute hh ll
      K.LabelRelative -> K.Relative ll
      K.LabelIndirect -> K.Indirect hh ll
      K.LabelZeroPage -> K.ZeroPage ll

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

-- | I think this is dead?
labelAddressingModeProgramCount :: K.LabelAddressing -> Word16
labelAddressingModeProgramCount = \case
  K.LabelIndirect -> 2
  K.LabelRelative -> 1
  K.LabelAbsolute -> 2
  K.LabelZeroPage -> 1
