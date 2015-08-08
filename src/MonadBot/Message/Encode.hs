{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Message.Encode
    ( encode
    ) where

import qualified Data.Text as T

import MonadBot.Message

-- | Encodes a message into Text to send down the wire.
encode :: Message -> T.Text
encode (Message Nothing c ps) =
    encodeCommand c <> encodeParams ps
encode (Message (Just p) c ps) =
    encodePrefix p <> encodeCommand c <> encodeParams ps
    --
-- | Encodes a prefix to Text
encodePrefix :: Prefix -> T.Text
encodePrefix (ServerPrefix sp) =
    ":" <> sp <> " "
encodePrefix (UserPrefix n (Just u) (Just h)) =
    ":" <> n <> "!" <> u <> "@" <> h <> " "

-- | Encodes a command to Text
encodeCommand :: T.Text -> T.Text
encodeCommand = (<> " ")

-- | Encodes params to Text
encodeParams :: [T.Text] -> T.Text
encodeParams p =
    ":" <> T.unwords p <> "\r\n"
