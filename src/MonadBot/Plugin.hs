{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module MonadBot.Plugin
    where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as T

import           MonadBot.Message
import           MonadBot.Types

