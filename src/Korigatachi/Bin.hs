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
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.List as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import Korigatachi.Assembly.Operand qualified as K
import Korigatachi.Atari.Model qualified as K
import Korigatachi.Control qualified as K
import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K
import Prelude hiding (and, read)

bin :: K.Hane K.Resolve K.Bin ()
bin = K.do
  (K.Resolve {..}) <- K.get
  K.log K.Info "Generating binary..."
  K.put $ K.Bin {K.binOutput = BS.empty}
  let
    word12 :: Word16 -> Word16
    word12 = (.&. 0x0FFF)
    splitWord w16 = let (ll, hh) = K.splitWord16 w16 in BS.pack [ll, hh]
    splitLong w32 =
      let (ll, lh, hl, hh) = K.splitWord32asWord8 w32 in BS.pack [ll, lh, hl, hh]
    genBinary :: K.Statement -> K.Hane K.Bin K.Bin ()
    genBinary statement = K.do
      case statement of
        K.Org w16 ->
          K.modify $ \bn@(K.Bin {..}) ->
            bn {K.binOutput = binOutput <> BS.replicate ((fromIntegral $ word12 w16) - BS.length binOutput) 0xFF}
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
  traverse_ genBinary resolveStatements
  K.log K.Info "Binary generated. Have a nice day."

instructBinary :: K.Shorthand -> K.Operand -> BS.ByteString
instructBinary sh opr =
  BS.pack $
    let
      oprName = operandToAddrModeName opr
    in
      case K.allInstructions Map.!? sh of
        Nothing -> [0x00]
        Just instructions ->
          case List.filter (\ins -> ins.addressingMode == oprName) instructions of
            [] -> [0x00]
            (ins : _) -> [ins.opcode] <> operandToWord8sLE opr

operandToAddrModeName :: K.Operand -> T.Text
operandToAddrModeName K.Accumulator = "Accumulator"
operandToAddrModeName K.Implied = "Implied"
operandToAddrModeName (K.Immediate _) = "Immediate"
operandToAddrModeName (K.IndirectX _) = "IndirectX"
operandToAddrModeName (K.IndirectY _) = "IndirectY"
operandToAddrModeName (K.Relative _) = "Relative"
operandToAddrModeName (K.ZeroPage _) = "ZeroPage"
operandToAddrModeName (K.ZeroPageX _) = "ZeroPageX"
operandToAddrModeName (K.ZeroPageY _) = "ZeroPageY"
operandToAddrModeName (K.Absolute _ _) = "Absolute"
operandToAddrModeName (K.AbsoluteX _ _) = "AbsoluteX"
operandToAddrModeName (K.AbsoluteY _ _) = "AbsoluteY"
operandToAddrModeName (K.Indirect _ _) = "Indirect"
operandToAddrModeName _ = ""

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
