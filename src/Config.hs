module Config
    (
    webServerPort,
    dataBaseHost,
    dataBaseUser,
    dataBasePassword
    ) where

import System.Environment (lookupEnv)

webServerPort :: IO Int
webServerPort = do
    str <- getEnvWithDefault "HTTP_PORT" "18080"
    return (read str :: Int)

dataBaseHost :: IO String
dataBaseHost =
    getEnvWithDefault "DB_HOST" "172.17.0.1"

dataBaseUser :: IO String
dataBaseUser =
    getEnvWithDefault "DB_USER" "neo4j"

dataBasePassword :: IO String
dataBasePassword =
    getEnvWithDefault "DB_PWD" "j4oen"

getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name def = do
    maybeValue <- lookupEnv name
    case maybeValue of
        Just value ->
            return value
        Nothing ->
            return def

