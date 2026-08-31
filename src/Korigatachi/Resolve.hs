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
  K.modify $
    \_ ->
      K.Resolve
        { K.resolveStatements = Seq.empty
        , K.resolveLabels = Seq.empty
        , K.resolveCodegen = Seq.empty
        , K.resolveProgramCounter = 0
        }
  let
    resolveStatement :: K.Statement -> K.Hane K.Resolve K.Resolve ()
    resolveStatement statement =
      K.modify $
        \rsv@(K.Resolve {..}) ->
          case statement of
            K.Instruct sh opr ->
              case opr of
                K.Label labelAddrModes lb ->
                  rsv
                    { K.resolveCodegen = resolveCodegen Seq.|> renderStatement statement
                    , K.resolveProgramCounter = resolveProgramCounter + operandToProgramCount opr
                    , K.resolveStatements =
                        resolveStatements
                          Seq.|> (K.Instruct sh $ resolveLabel resolveLabels labelAddrModes lb)
                    }
                _ ->
                  rsv
                    { K.resolveCodegen = resolveCodegen Seq.|> renderStatement statement
                    , K.resolveProgramCounter = resolveProgramCounter + operandToProgramCount opr
                    , K.resolveStatements = resolveStatements Seq.|> statement
                    }
            K.TopLevelLabel label ->
              rsv
                { K.resolveCodegen = resolveCodegen Seq.|> renderStatement statement
                , K.resolveLabels = resolveLabels Seq.|> (resolveProgramCounter, label)
                , K.resolveStatements = resolveStatements Seq.|> statement
                }
            K.Org w16 ->
              rsv
                { K.resolveCodegen = resolveCodegen Seq.|> renderStatement statement
                , K.resolveProgramCounter = w16
                , K.resolveStatements = resolveStatements Seq.|> statement
                }
            _ ->
              rsv
                { K.resolveCodegen = resolveCodegen Seq.|> renderStatement statement
                , K.resolveStatements = resolveStatements Seq.|> statement
                }
  void $ traverse resolveStatement statements
  pure ()

resolveLabel :: Seq.Seq (Word16, T.Text) -> [K.LabelAddressing] -> T.Text -> K.Operand
resolveLabel labels labelAddressing toResolve =
  let
    found = Seq.lookup 0 $ Seq.filter (\(_, lb) -> lb == toResolve) labels
  in
    case (found, labelAddressing) of
      (Nothing, _) -> K.Label [] "FUCK"
      (_, []) -> K.Label [] "FUCK"
      (Just (addr, _), (a : _)) ->
        -- go away
        reifyLabel a addr

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
