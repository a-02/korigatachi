{-# LANGUAGE OverloadedStrings #-}

module Korigatachi.Control where

-- base

import Data.Text (pack)
import Data.Word (Word16, Word8)
import Numeric

-- optics
import Optics

import Korigatachi.Model
import Korigatachi.Monad
import Korigatachi.Types

hex8 :: Getter Word8 String
hex8 = to $ \w8 -> showHex w8 ""

hex16 :: Getter Word16 String
hex16 = to $ \w16 -> showHex w16 ""

when :: Switch -> Korigatachi () -> Korigatachi ()
when s k = case s of
  On -> k
  Off -> ixpure ()

logErr :: String -> Korigatachi ()
logErr = flip katteyomi ("") . pack
