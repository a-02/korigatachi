{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Korigatachi.Comonad where

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b

data Store s a = Store
  { accessor :: s -> a
  , index :: s
  }

seek :: s -> Store s a -> Store s a
seek s (Store acc _) = Store acc s

instance Functor (Store s) where
  fmap :: forall a b. (a -> b) -> Store s a -> Store s b
  fmap f (Store acc idx) = Store (f . acc) idx  

instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store acc idx) = acc idx

  duplicate :: Store s a -> Store s (Store s a)
  duplicate (Store acc idx) = Store (Store acc) idx

  extend :: (Store s a -> b) -> Store s a -> Store s b
  extend f = fmap f . duplicate


