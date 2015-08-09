module MonadBot.Utility
    ( ppMessage
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import MonadBot.Message (Message (..), Prefix (..))

ppMessage :: Message -> Text
ppMessage msg =
    case prefix msg of
      (Just (ServerPrefix p)) ->
          T.pack $ printf "[%s] %s %s" (T.unpack p) (T.unpack $ command msg) (T.unpack . T.unwords $ params msg)
      (Just (UserPrefix p _ _)) ->
          T.pack $ printf "[%s] %s %s" (T.unpack p) (T.unpack $ command msg) (T.unpack . T.unwords $ params msg)
      Nothing ->
          T.pack $ printf "%s %s" (T.unpack $ command msg) (T.unpack . T.unwords $ params msg)
