{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Korigatachi.Assembly.Control.Internal where

import Data.Text qualified as T

import Korigatachi.Types qualified as K

-- | Hey mom, look! Inaccessible constructors!
destructIntLike :: K.ASMArg K.IntLike -> Word
destructIntLike = \case
  K.AAInt i -> (fromIntegral i)
  K.AAByte b -> (fromIntegral b)
  K.AAWord w -> (fromIntegral w)
  K.AALong l -> (fromIntegral l)

-- | Hey mom, look! Inaccessible constructors!
destructTextLike :: K.ASMArg K.TextLike -> T.Text
destructTextLike = \case
  K.AAExpr e -> e
