module Configuration (loadConfig) where

import Configuration.Dotenv
import Database.PostgreSQL.Simple
import Validation ( defaultValidator, Validator )


getValue :: [(String, String)] -> String -> IO String
getValue pairs key = case found of
        Just value -> return value
        Nothing -> do
            putStrLn $ "\"" ++ key ++ "\" not found in the environment!"
            return ""
    where
        found = lookup key pairs


loadConfig :: IO (String, Connection, Validator)
loadConfig = do
    envValues <- parseFile ".env"
    jwtSecret <- getValue envValues "jwtSecret"
    dbHost <- getValue envValues "dbHost"
    dbPort <- getValue envValues "dbPort"
    dbName <- getValue envValues "dbName"
    dbUser <- getValue envValues "dbUser"
    dbPassword <- getValue envValues "dbPassword"
    let dbInfo = defaultConnectInfo {
            connectHost = dbHost,
            connectPort = read dbPort,
            connectDatabase = dbName,
            connectUser = dbUser,
            connectPassword = dbPassword
        }
    dbCon <- connect dbInfo
    putStrLn $ "Connected to DB \"" ++ dbName ++ 
        "\" with user \"" ++ dbUser ++
        "\" on " ++ dbHost ++ ":" ++ dbPort
    return (jwtSecret, dbCon, defaultValidator)
