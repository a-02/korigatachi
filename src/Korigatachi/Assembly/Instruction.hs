{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Korigatachi.Assembly.Instruction where

import Prelude hiding (and)

import Korigatachi.Assembly.Instruction.TH

import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K

$(instructions)
