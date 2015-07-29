{-# LANGUAGE OverloadedStrings #-}
module MonadBot.MessageParser
    ( encode
    , decode
    )
    where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import MonadBot.Message

-- | This should probably be somewhere else, module-wise.
decode :: T.Text -> Either String Message
-- decode = eitherResult . parse messageP
decode = parseOnly (messageP <* endOfInput)

-- | Encodes a message into Text to send down the wire.
encode :: Message -> T.Text
encode (Message Nothing c ps) =
    encodeCommand c <> encodeParams ps
encode (Message (Just p) c ps) =
    encodePrefix p <> encodeCommand c <> encodeParams ps

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
    T.unwords p <> "\r\n"

-- | Test messages from sample session
testMessages :: IO [Text]
testMessages =
     (map T.init . T.lines) <$> TIO.readFile "irc.out"

-- | Test parser and prints to stdout cleanly
testParser :: IO ()
testParser = do
    m <- testMessages
    mapM_ (putStrLn . (++ "\n") . show . parseOnly messageP) m

{- | Concatenation combinator
(<+>) :: Parser Text -> Parser Text -> Parser Text
a <+> b = liftA2 T.append a b

-- | Cons combinator
(<:>) :: Parser Char -> Parser Text -> Parser Text
a <:> b = liftA2 T.cons a b
-}

messageP :: Parser Message
messageP = do
    prefix'  <- optional $ char ':' *> prefixP <* space
    command' <- commandP
    space
    params'  <- paramsP
    return $ Message prefix' command' params'

-- | Parses params
paramsP :: Parser [Text]
paramsP =
    T.words . T.dropWhile (== ':') <$> takeText

-- | Parses commands
commandP :: Parser T.Text
commandP =
    takeWhile1 (inClass "a-zA-Z") <|>
        takeWhile1 (`elem` ['0' .. '9'])

-- | Parses Prefix
prefixP :: Parser Prefix
prefixP =
    userPrefixP <|> serverPrefixP

-- | Parses server prefix
serverPrefixP :: Parser Prefix
serverPrefixP =
    ServerPrefix <$> takeTill (== ' ')

-- | Parses nick prefix
userPrefixP :: Parser Prefix
userPrefixP = do
    nick' <- takeWhile (`notElem` (" !" :: String))
    char '!'
    user' <- optional $ takeTill (== '@')
    char '@'
    host' <- optional $ takeTill (== ' ')
    return $ UserPrefix nick' user' host'
