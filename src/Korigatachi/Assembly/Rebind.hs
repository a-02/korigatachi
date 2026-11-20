module Korigatachi.Assembly.Rebind where

import Prelude as Prelude

import Data.Text qualified as T

import Korigatachi.Types (ASMArg (..), IntLike, TextLike)

fromInteger :: Integer -> ASMArg IntLike
fromInteger i
  | i > 0 && i <= 255 = AAByte (Prelude.fromInteger i)
  | i > 255 && i <= 65535 = AAWord (Prelude.fromInteger i)
  | i > 65536 && i <= (2 ^ (32 :: Integer) - 1) = AALong (Prelude.fromInteger i)
  | otherwise = AAInt i

fromString :: String -> ASMArg TextLike
fromString str = AAExpr (T.pack str)
