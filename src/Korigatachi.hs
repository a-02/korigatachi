{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}

module Korigatachi where

import Data.Sequence qualified as Seq
import Korigatachi.Bin qualified as K.Bin
import Korigatachi.Monad qualified as K
import Korigatachi.Resolve qualified as K.Resolve
import Korigatachi.Types qualified as K
import Data.ByteString qualified as ByteString
import qualified Data.Text as T

-- | Render a Korigatachi program as assembly.
render :: K.Assembly () -> IO T.Text
render asm = do
  let
    prog = K.do
      asm
      K.Resolve.resolve
  (_, _, kty) <-
    K.runRWIT prog (K.Env K.Warn) (K.Assemble Seq.empty)
  pure kty.codegen

-- | Assemble a Korigatachi program to binary.
assemble :: K.Assembly () -> IO ByteString.ByteString
assemble asm = do
  let
    prog = K.do
      asm
      K.Resolve.resolve
      K.Bin.bin
      K.get
  ((K.Bin bs), _, _) <-
    K.runRWIT prog (K.Env K.Warn) (K.Assemble Seq.empty)
  pure bs
