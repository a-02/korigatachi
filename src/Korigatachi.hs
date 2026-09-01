{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}

module Korigatachi where

-- import Data.ByteString qualified as ByteString
-- import Data.Text.IO.Utf8 qualified as Text.IO.Utf8
-- import Data.Vector.Sized qualified as Sized

-- -- import Korigatachi.Assembly.CodeGen
import Korigatachi.Monad qualified as K
import qualified Korigatachi.Assembly.Demo as K.Demo
import qualified Korigatachi.Resolve as K.Resolve
import qualified Data.Sequence as Seq
import qualified Korigatachi.Types as K
-- import Korigatachi.Types

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
  _ <-
    K.runRWIT prog (K.Env K.Warn) (K.Assemble Seq.empty)
      
      
  -- let
  --   bin = ByteString.pack $ Sized.toList atr.rom.memory4k
  -- ByteString.writeFile "start.bin" bin
  -- -- Text.IO.Utf8.writeFile "fakesource.hs" renderedSource
  -- Text.IO.Utf8.writeFile "korigatachi.log" kty.logs
  -- Text.IO.Utf8.writeFile "start.asm" kty.codegen
  pure ()
