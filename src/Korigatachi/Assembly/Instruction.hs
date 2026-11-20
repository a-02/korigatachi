{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Korigatachi.Assembly.Instruction where

import Prelude hiding (and)

import Korigatachi.Assembly.Instruction.TH

import Korigatachi.Monad qualified as K
import Korigatachi.Types qualified as K

$(instructions)
