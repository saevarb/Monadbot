{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Message.Encode
    ( encode
    ) where

import qualified Data.Text as T

import MonadBot.Message

-- | Encodes a message into Text to send down the wire.
encode :: Message -> T.Text
encode (Message Nothing c ps) =
    encodeCommand c <> encodeParams ps <> "\r\n"
encode (Message (Just p) c ps) =
    encodePrefix p <> encodeCommand c <> encodeParams ps <> "\r\n"

-- | Encodes a prefix to Text
encodePrefix :: Prefix -> T.Text
encodePrefix (ServerPrefix sp) =
    ":" <> sp <> " "
encodePrefix (UserPrefix n (Just u) (Just h)) =
    ":" <> n <> "!" <> u <> "@" <> h <> " "
encodePrefix _ =
    error "encodePrefix: This should never happen. Please report this as a bug."

-- | Encodes a command to Text
encodeCommand :: T.Text -> T.Text
encodeCommand = (<> " ")

-- | Encodes params to Text
encodeParams :: [T.Text] -> T.Text
encodeParams (s:m:e) =
    T.unwords $ [s, ":" <> m] ++ e
encodeParams p =
    T.unwords $ p
