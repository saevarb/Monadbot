module MonadBot.Writer
    ( Writer
    , makeWriter
    ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Concurrent.STM
import Control.Monad

import MonadBot.Message
import MonadBot.Types
