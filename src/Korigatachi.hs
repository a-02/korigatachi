{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}

module Korigatachi where

-- import Data.ByteString qualified as ByteString
-- import Data.Text.IO.Utf8 qualified as Text.IO.Utf8
-- import Data.Vector.Sized qualified as Sized

-- -- import Korigatachi.Assembly.CodeGen

import Data.Sequence qualified as Seq
import Korigatachi.Assembly.Demo qualified as K.Demo
import Korigatachi.Bin qualified as K.Bin
import Korigatachi.Monad qualified as K
import Korigatachi.Resolve qualified as K.Resolve
import Korigatachi.Types qualified as K

-- import Korigatachi.Types

import Data.ByteString qualified as ByteString
import Data.Text.IO.Utf8 qualified as Text.IO.Utf8
import Text.Show.Pretty qualified as Pretty

korigatachi :: IO ()
korigatachi = do
  let
    prog = K.do
      K.Demo.sample
      assemble <- K.get
      K.liftIO $ Pretty.pPrint assemble
      K.Resolve.resolve
      res <- K.get
      K.liftIO $ Pretty.pPrint res
      K.Bin.bin
      (K.Bin bs) <- K.get
      K.ixpure bs
  (bs, _, kty) <-
    K.runRWIT prog (K.Env K.Warn) (K.Assemble Seq.empty)
  Text.IO.Utf8.writeFile "korigatachi.log" kty.logs
  Text.IO.Utf8.writeFile "start.asm" kty.codegen
  ByteString.writeFile "start.bin" bs
  pure ()
