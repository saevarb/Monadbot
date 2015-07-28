module MonadBot.Writer
    ( Writer 
    , makeWriter
    ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Concurrent.STM
import Control.Monad

import MonadBot.Message

type Writer = Message -> IO ()

makeWriter :: TQueue Message -> Writer
makeWriter q m =
    atomically $ writeTQueue q m


