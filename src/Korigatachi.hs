{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Korigatachi where

import Data.ByteString qualified as ByteString
import Data.Text.IO.Utf8 qualified as Text.IO.Utf8
import Data.Vector.Sized qualified as Sized
import Korigatachi.Demo as Demo
import Korigatachi.Model (emptyAtari)
import Korigatachi.Monad
import Korigatachi.Types

korigatachi :: IO ()
korigatachi = do
  (_, atr, kty) <- runRWIT Demo.sample (Env On Off Off Test) emptyAtari
  let
    bin = ByteString.pack $ Sized.toList atr.rom.memory4k
  ByteString.writeFile "start.bin" bin
  Text.IO.Utf8.writeFile "korigatachi.log" kty.logs
  Text.IO.Utf8.writeFile "start.asm" kty.codegen
  pure ()
