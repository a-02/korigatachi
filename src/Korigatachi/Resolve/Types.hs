module Korigatachi.Resolve.Types where

data LabelAddressing
  = LabelIndirect
  | LabelRelative
  | LabelAbsolute
  | LabelZeroPage
  deriving (Eq, Ord, Show)
