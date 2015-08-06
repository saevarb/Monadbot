module MonadBot.Message.Decode
    ( decode
    )
    where
import qualified Data.Text as T

import Data.Attoparsec.Text (eitherResult, parse)

import MonadBot.Message.Decode.Parser
import MonadBot.Message

-- | This should probably be somewhere else, module-wise.
decode :: T.Text -> Either String Message
decode = eitherResult . parse messageP
