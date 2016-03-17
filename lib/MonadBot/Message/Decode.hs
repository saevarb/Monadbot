module MonadBot.Message.Decode
    ( decode
    )
    where
import qualified Data.Text as T

import Data.Attoparsec.Text (parseOnly, endOfInput)

import MonadBot.Message.Decode.Parser
import MonadBot.Message

-- | This should probably be somewhere else, module-wise.
decode :: T.Text -> Either String Message
-- decode = eitherResult . parse messageP
decode = parseOnly (messageP <* endOfInput)
