{-# LANGUAGE RebindableSyntax #-}

module Korigatachi.Assembly.Rebind where

import Prelude hiding (fromInteger)

fromInteger = Directive d
