newtype Remote a = Remote { runRemote :: String -> (Bool, Maybe a) }

