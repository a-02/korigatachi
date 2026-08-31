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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Use $>" -}

module Korigatachi.Resolve where

import Data.Sequence qualified as Seq

-- import Data.Text qualified as T
-- import Data.Word (Word16)
-- import Korigatachi.Control qualified as K

import Data.Functor (void)
import Data.Text qualified as T
import Data.Word (Word16)
import Korigatachi.Control qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K
import Optics ((^.))
import Prelude hiding (and, read)

resolve :: K.Hane K.Assemble K.Resolve ()
resolve = K.do
  K.modify $
    \(K.Assemble statements) ->
      K.Resolve
        { K.resolveStatements = statements
        , K.resolveLabels = Seq.empty
        , K.resolveCodegen = Seq.empty
        , K.resolveProgramCounter = 0
        }
  res <- K.get
  let
    statements = K.resolveStatements res
    resolveStatement :: K.Statement -> K.Hane K.Resolve K.Resolve ()
    resolveStatement statement =
      K.modify $
        \(K.Resolve {..}) ->
          case statement of
            K.Org w16 ->
              K.Resolve
                { K.resolveCodegen = resolveCodegen Seq.|> undefined
                }
  void $ traverse resolveStatement statements
  pure ()

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
