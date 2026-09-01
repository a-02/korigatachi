{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Korigatachi.Assembly.Instruction where

import Control.Applicative
import Control.Monad ((>=>))
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Korigatachi.Assembly.Control qualified as K
import Korigatachi.Assembly.Instruction.TH
import Korigatachi.Assembly.Operand
import Korigatachi.Control qualified as K
import Korigatachi.Types qualified as K

$(generateInstructions)
