


import Web.Scotty


-- create system, automatically creates web server


systemWebServer ∷ Universe → System → IO ()
systemWebServer universe system = do



server ∷ IO ()
server appName sysName = scotty 3000 

  where

    getValue = get (regex (appName ++ "/" ++ sysName



