module Korigatachi where

import Control.Monad (void)
import Korigatachi.Assembly as Assembly
import Korigatachi.Model (Env (..), Switch (..), emptyAtari)
import Korigatachi.Monad

korigatachi :: IO ()
korigatachi = void $ runRWIT Assembly.start (Env On Off Off) emptyAtari
