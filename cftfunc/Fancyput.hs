-- Input with fancy effects, Fancyput
module Fancyput
( putconch, putcon
) where

import Cftdef (dejustst)
import System.Console.Haskeline
import System.IO

putconch :: MonadException m => String -> m String
putconch = fmap dejustst . runInputT defaultSettings . getInputLine
putcon :: MonadException m => m String
putcon = putconch "> "
