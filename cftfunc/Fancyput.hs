-- Input with fancy effects, Fancyput
module Fancyput
( putconch, putcon
) where

import System.Console.Haskeline
import System.IO

putconch :: MonadException m => String -> m String
putconch = fmap dejust . runInputT defaultSettings . getInputLine
putcon :: MonadException m => m String
putcon = putconch "> "
dejust :: Maybe String -> String
dejust (Just x) = x
dejust Nothing  = ""
