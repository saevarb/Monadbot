module MonadBot.Plugins
    ( allPlugins
    ) where
import qualified MonadBot.Plugins.Ping as Ping
import qualified MonadBot.Plugins.Join as Join
import qualified MonadBot.Plugins.Test as Test

allPlugins =
    [ Ping.plugin
    , Join.plugin
    , Test.plugin
    ]
