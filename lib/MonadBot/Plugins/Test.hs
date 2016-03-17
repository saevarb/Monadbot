{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Plugins.Test
    ( plugin
    ) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Text as T
import           System.Random

import MonadBot.Plugin.Development


plugin :: Plugin ()
plugin = mkSimplePlugin "Test handler" []
