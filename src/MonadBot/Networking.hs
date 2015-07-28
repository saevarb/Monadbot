{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module MonadBot.Networking where

import           Conduit
import           Data.Conduit
import qualified Data.Conduit.Binary     as CB
import qualified Data.Conduit.List       as CL
import           Data.Conduit.Network
import qualified Data.Conduit.Text       as CT
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO

import           MonadBot.Config
import           MonadBot.Message
import           MonadBot.Message.Encode
import           MonadBot.Types

mkAuthMessage :: Environment -> [Message]
mkAuthMessage env = catMaybes
    [ (serverPass . server $ env) >>= \p -> return $ Message Nothing "PASS" [p]
    , return $ Message Nothing "NICK" [myNick env]
    , return $ Message Nothing "USER" [myNick env, "0", "*", ":fake"]
    ]


doStuff :: Environment -> Conduit Text IO Text
doStuff (Environment { .. }) =
    awaitForever $ \t -> do
        liftIO $ logger t
        yield t

botApp env appData = do
    initialize env $= CT.encode CT.utf8 $$ appSink appData
    appSource appData
    $= CB.lines
    $= CT.decode CT.utf8
    $= doStuff env
    $= CT.encode CT.utf8
    $$ appSink appData

initialize env =
    CL.sourceList $ map encode (mkAuthMessage env)
