{-# LANGUAGE DerivingStrategies #-}

module Korigatachi.Resolve.Types where

data LabelAddressing
  = LabelIndirect
  | LabelRelative
  | LabelAbsolute
  deriving stock (Eq, Ord, Show)
