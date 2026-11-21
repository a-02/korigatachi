{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Korigatachi.Assembly.Control.Internal where

import Data.Word

import Data.Text qualified as T

import Korigatachi.Types qualified as K

-- | Hey mom, look! Inaccessible constructors!
destructIntLike :: K.ASMArg K.IntLike -> Word
destructIntLike = \case
  K.AAInt i -> (fromIntegral i)
  K.AAByte b -> (fromIntegral b)
  K.AAWord w -> (fromIntegral w)
  K.AALong l -> (fromIntegral l)

sizeASMArg :: Word -> K.ASMArg K.IntLike
sizeASMArg w
  | w < 2 ^ (8 :: Word) = K.AAByte $ fromIntegral w
  | w < 2 ^ (16 :: Word) = K.AAWord $ fromIntegral w
  | w < 2 ^ (32 :: Word) = K.AALong $ fromIntegral w
  | otherwise = K.AAInt $ fromIntegral w

-- | Hey mom, look! Inaccessible constructors!
destructTextLike :: K.ASMArg K.TextLike -> T.Text
destructTextLike = \case
  K.AAExpr e -> e

renderASMArg :: forall t. K.Gukei t => K.ASMArg t -> T.Text
renderASMArg asmArg = case K.gukei @t of
  K.IntTag -> T.show (destructIntLike asmArg)
  K.TextTag -> destructTextLike asmArg

binaryASMOperation
  :: forall a b
   . (K.Gukei a, K.Gukei b)
  => (Word -> Word -> Word)
  -> T.Text
  -> K.ASMArg a
  -> K.ASMArg b
  -> K.ASMArg (K.ASMBinOp a b)
binaryASMOperation binOp sym argA argB =
  let textClause = K.AAExpr $ renderASMArg argA <> sym <> renderASMArg argB
   in case (K.gukei @a, K.gukei @b) of
        (,) K.IntTag K.IntTag -> sizeASMArg $ destructIntLike argA `binOp` destructIntLike argB
        (,) K.IntTag K.TextTag -> textClause
        (,) K.TextTag K.IntTag -> textClause
        (,) K.TextTag K.TextTag -> textClause

orgUndersizeWarning :: Word8 -> T.Text
orgUndersizeWarning w8 = "`org` called with 8-bit arg \"" <> T.show w8 <> "\", expected 16-bit."

wordUndersizeWarning :: Word8 -> T.Text
wordUndersizeWarning w8 = "`word` called with 8-bit arg \"" <> T.show w8 <> "\", expected 16-bit."

orgOversizeWarning :: Word32 -> T.Text
orgOversizeWarning w32 =
  "`org` called with 32-bit arg \""
    <> T.show w32
    <> "\", expected 16-bit. Ignoring!"

wordOversizeWarning :: Word32 -> T.Text
wordOversizeWarning w32 =
  "`word` called with 32-bit arg \""
    <> T.show w32
    <> "\", expected 16-bit. Writing as two 16-bit words."
