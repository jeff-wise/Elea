


import Web.Scotty


-- create system, automatically creates web server


systemWebServer ∷ Universe → System → IO ()
systemWebServer universe system = do



server ∷ IO ()
server appName sysName = scotty 3000 

  where

    getValue = get (regex (appName ++ "/" ++ sysName



systemWebHandler ∷ AppId → SystemId → System → ScottyM ()
systemWebHandler appId sysId system = do
  let route = function $ systemRoute appId sysId
  get route $ do
    -- check if value id is in system





systemRoute ∷ AppId → SystemId → Request → Maybe [Param]
systemRoute appId sysId request = do
  let (routeApp : routeSys : routeVal : rest) = pathInfo request 
  guard $ (routeApp == appId && routeSys == sysId)
  return [("value_id", routeVal), ("value_path", rest)]
  
  

