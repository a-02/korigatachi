module Korigatachi where

import Korigatachi.Assembly as Assembly
import Korigatachi.Monad
import Korigatachi.Model (Env(..), emptyAtari, Switch (..))
import Control.Monad (void)

korigatachi :: IO ()
korigatachi = void $ runRWIT Assembly.start (Env On Off Off) emptyAtari
