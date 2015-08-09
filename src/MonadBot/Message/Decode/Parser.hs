{-# LANGUAGE OverloadedStrings #-}
module MonadBot.Message.Decode.Parser
    ( messageP
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

-- -- | Test messages from sample session
-- testMessages :: IO [Text]
-- testMessages =
--      (map T.init . T.lines) <$> TIO.readFile "irc.out"

-- -- | Test parser and prints to stdout cleanly
-- testParser :: IO ()
-- testParser = do
--     m <- testMessages
--     mapM_ (putStrLn . (++ "\n") . show . parseOnly messageP) m

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
