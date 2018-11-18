-- Input with fancy effects, Fancyput
module Fancyput
( putconch, putcon
) where

import System.Console.Haskeline
import System.IO

-- remove Just from Maybe String
dejustst :: Maybe String -> String
dejustst (Just x) = x
dejustst Nothing  = ""
putconch :: MonadException m => String -> m String
putconch = fmap dejustst . runInputT defaultSettings . getInputLine
putcon :: MonadException m => m String
putcon = putconch "> "
