import MonadBot

main :: IO ()
main = readConfig "monadbot.yaml" >>= runBot
