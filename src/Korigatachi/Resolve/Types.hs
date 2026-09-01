module Korigatachi.Resolve.Types where

data LabelAddressing
  = LabelIndirect
  | LabelRelative
  | LabelAbsolute
  deriving (Eq, Ord, Show)
