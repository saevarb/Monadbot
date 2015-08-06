module MonadBot.Logging
    where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Concurrent.STM
import Control.Monad


type Logger = TQueue Text

logWorker :: TQueue Text -> IO ()
logWorker q = forever $ do
    msg <- atomically $ readTQueue q
    TIO.putStrLn msg
