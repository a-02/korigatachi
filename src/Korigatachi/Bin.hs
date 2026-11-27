{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}

module Korigatachi.Bin where

import Data.Bits ((.&.))
import Data.Foldable (traverse_)
import Data.List as List
import Data.Word (Word16, Word8)
import Prelude hiding (and, read)

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Korigatachi.Assembly.Operand qualified as K
import Korigatachi.Atari.Model qualified as K
import Korigatachi.Control qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K

bin :: K.Hane K.Resolver K.Bin ()
bin = K.do
  (K.Resolver {..}) <- K.get
  K.log K.Info "Generating binary..."
  K.put $ K.Bin {K.binOutput = BS.empty}
  let word12 :: Word16 -> Word16
      word12 = (.&. 0x0FFF)
      splitWord w16 = let (ll, hh) = K.splitWord16 w16 in BS.pack [ll, hh]
      splitLong w32 =
        let (ll, lh, hl, hh) = K.splitWord32asWord8 w32 in BS.pack [ll, lh, hl, hh]
      genBinary :: K.Statement -> K.Hane K.Bin K.Bin ()
      genBinary statement = K.do
        case statement of
          K.Org w16 ->
            K.modify $ \bn@(K.Bin {..}) ->
              bn
                { K.binOutput =
                    binOutput
                      <> BS.replicate ((fromIntegral $ word12 w16) - BS.length binOutput) 0xFF
                }
          K.Long w32 ->
            K.modify $ \bn@(K.Bin {..}) ->
              bn {K.binOutput = binOutput <> splitLong w32}
          K.Word w16 ->
            K.modify $ \bn@(K.Bin {..}) ->
              bn {K.binOutput = binOutput <> splitWord w16}
          K.Byte w8 ->
            K.modify $ \bn@(K.Bin {..}) ->
              bn {K.binOutput = binOutput `BS.snoc` w8}
          K.Instruct sh opr ->
            K.modify $ \bn@(K.Bin {..}) ->
              bn {K.binOutput = binOutput <> (instructBinary sh opr)}
          _ -> pure ()
  traverse_ genBinary resolverStatements
  K.log K.Info "Binary generated. Have a nice day."

instructBinary :: K.Shorthand -> K.Operand -> BS.ByteString
instructBinary sh opr =
  BS.pack $
    let oprName = operandToAddrMode opr
     in case K.allInstructions Map.!? sh of
          Nothing -> [0x00]
          Just instructions ->
            case List.filter (\ins -> ins.addressingMode == oprName) instructions of
              [] -> [0x00]
              (ins : _) -> [ins.opcode] <> operandToWord8sLE opr

operandToAddrMode :: K.Operand -> K.AddressingMode
operandToAddrMode K.Accumulator = K.AMAccumulator
operandToAddrMode K.Implied = K.AMImplied
operandToAddrMode (K.Immediate _) = K.AMImmediate
operandToAddrMode (K.IndirectX _) = K.AMIndirectX
operandToAddrMode (K.IndirectY _) = K.AMIndirectY
operandToAddrMode (K.Relative _) = K.AMRelative
operandToAddrMode (K.ZeroPage _) = K.AMZeroPage
operandToAddrMode (K.ZeroPageX _) = K.AMZeroPageX
operandToAddrMode (K.ZeroPageY _) = K.AMZeroPageY
operandToAddrMode (K.Absolute _ _) = K.AMAbsolute
operandToAddrMode (K.AbsoluteX _ _) = K.AMAbsoluteX
operandToAddrMode (K.AbsoluteY _ _) = K.AMAbsoluteY
operandToAddrMode (K.Indirect _ _) = K.AMIndirect
operandToAddrMode _ = K.AMUnresolved

operandToWord8sLE :: K.Operand -> [Word8]
operandToWord8sLE K.Accumulator = []
operandToWord8sLE K.Implied = []
operandToWord8sLE (K.Immediate w8) = [w8]
operandToWord8sLE (K.IndirectX w8) = [w8]
operandToWord8sLE (K.IndirectY w8) = [w8]
operandToWord8sLE (K.Relative w8) = [w8]
operandToWord8sLE (K.ZeroPage w8) = [w8]
operandToWord8sLE (K.ZeroPageX w8) = [w8]
operandToWord8sLE (K.ZeroPageY w8) = [w8]
operandToWord8sLE (K.Absolute hh ll) = [ll, hh]
operandToWord8sLE (K.AbsoluteX hh ll) = [ll, hh]
operandToWord8sLE (K.AbsoluteY hh ll) = [ll, hh]
operandToWord8sLE (K.Indirect hh ll) = [ll, hh]
operandToWord8sLE _ = []
