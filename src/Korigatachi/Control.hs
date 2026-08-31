{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}

module Korigatachi.Control where

-- containers

-- finite-typelits

-- text
import Data.Text qualified as T

-- sized-vector

-- base

import Prelude hiding (break)

-- korigatachi

import Data.Word
import Korigatachi.Monad as K
import Korigatachi.Types
import Numeric (showHex)
import Optics

-- | Write to the writer.
katteyomi :: T.Text -> T.Text -> Hane i i ()
katteyomi logMsg code = tell (Katteyomi logMsg code)

-- | Log a message regardless of loglevel.
logAny :: T.Text -> Hane i i ()
logAny logMsg = katteyomi (logMsg <> "\n") ""

-- | Output generated code to the writer.
codeGen :: T.Text -> Hane i i ()
codeGen code = katteyomi "" (code <> "\n")

{- | Log a message at a specific log level.
This will only log messages that are at or above
the current verbosity level of the program.
-}
log :: LogLevel -> T.Text -> Hane i i ()
log level logMsg = K.do
  currentLevel <- logLevel <$> K.ask
  if level >= currentLevel
    then logAny (T.show level <> logMsg)
    else K.ixpure ()

logErr :: String -> Hane i i ()
logErr = flip katteyomi ("") . T.pack

hex8 :: Getter Word8 String
hex8 = to $ \w8 -> showHex w8 ""

hex16 :: Getter Word16 String
hex16 = to $ \w16 -> showHex w16 ""
