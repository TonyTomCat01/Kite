module Log where

import Control.Exception
import Data.Either
import Data.Time
import System.IO
import Util

logVal logval = do
    time <- getCurrentTime
    _ <- try (appendFile logFile (string time)) :: IO (Either SomeException ())
    return ()
  where
    string time = "[" <> show time <> "] " <> logval <> "\n"
