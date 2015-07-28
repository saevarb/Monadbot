module MonadBot.Logging
    where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Concurrent.STM
import Control.Monad


type Logger = Text -> IO ()

makeLogger :: TQueue Text -> Logger
makeLogger q t = 
    atomically $ writeTQueue q t

logWorker :: TQueue Text -> IO ()
logWorker q = forever $ do
    msg <- atomically $ readTQueue q 
    TIO.putStrLn msg

