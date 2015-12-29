module MonadBot.Plugins
    ( allPlugins
    ) where
import MonadBot.Types

import qualified MonadBot.Plugins.Ping as Ping
import qualified MonadBot.Plugins.Join as Join
import qualified MonadBot.Plugins.Test as Test
import qualified MonadBot.Plugins.Darchoods as Darchoods
import qualified MonadBot.Plugins.Version as Version
import qualified MonadBot.Plugins.Nixers as Nixers
import qualified MonadBot.Plugins.Auth as Auth
import qualified MonadBot.Plugins.Url as Url
import qualified MonadBot.Plugins.Brainfuck as Brainfuck

allPlugins :: [Hide Plugin]
allPlugins =
    [ Enabled Ping.plugin
    , Enabled Join.plugin
    , Enabled Test.plugin
    , Enabled Darchoods.plugin
    , Enabled Version.plugin
    , Enabled Nixers.plugin
    , Enabled Auth.plugin
    , Disabled Url.plugin
    , Enabled Brainfuck.plugin
    ]
