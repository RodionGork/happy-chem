module Core
    ( entryPoint
    ) where

import System.IO
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8

import Network.Wai (responseLBS, Application, requestMethod, requestBody)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

entryPoint :: IO ()
entryPoint = do
    let port = 18080
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app request respond
    | requestMethod request == (C8.pack "POST")
    = doPost request respond
    | otherwise
    = doGet respond

doPost request respond = do
    c <- requestBody request
    writeFile "state.txt" (C8.unpack c)
    respond $ responseLBS
        status200
        []
        (LC8.pack "ya post\n")

doGet respond = do
    c <- readFile "state.txt"
    respond $ responseLBS
        status200
        []
        (LC8.pack $ "ya get: " ++ c ++ "\n")

