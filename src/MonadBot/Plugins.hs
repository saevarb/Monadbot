module MonadBot.Plugins
    ( allPlugins
    ) where
import MonadBot.Types

import qualified MonadBot.Plugins.Ping as Ping
import qualified MonadBot.Plugins.Join as Join
import qualified MonadBot.Plugins.Test as Test
import qualified MonadBot.Plugins.Version as Version
import qualified MonadBot.Plugins.Auth as Auth
import qualified MonadBot.Plugins.Brainfuck as Brainfuck

allPlugins :: [Hide Plugin]
allPlugins =
    [ Enabled Ping.plugin
    , Enabled Join.plugin
    , Enabled Test.plugin
    , Enabled Version.plugin
    , Enabled Auth.plugin
    , Enabled Brainfuck.plugin
    ]
