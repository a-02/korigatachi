{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Korigatachi.Assembly.Control.Internal where

import Data.Text qualified as T
import Korigatachi.Types qualified as K

asmWordEither :: K.ASMArg K.IntLike -> Either T.Text Word
asmWordEither = \case
  K.AAInt i -> Right (fromIntegral i)
  K.AAByte b -> Right (fromIntegral b)
  K.AAWord w -> Right (fromIntegral w)
  K.AALong l -> Right (fromIntegral l)
  K.AAExpr _ -> Left "How did you get here?"
