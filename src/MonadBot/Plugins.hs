module MonadBot.Plugins
    ( allPlugins
    ) where
import qualified MonadBot.Plugins.Ping as Ping

allPlugins =
    [ Ping.plugin
    ]
