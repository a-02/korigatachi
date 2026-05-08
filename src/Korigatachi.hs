module Korigatachi where

import Control.Monad (void)
import Korigatachi.Demo as Demo
import Korigatachi.Model (emptyAtari)
import Korigatachi.Monad
import Korigatachi.Types

korigatachi :: IO ()
korigatachi = void $ runRWIT Demo.start (Env On Off Off) emptyAtari
