module Core
    ( entryPoint
    ) where

import System.IO
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8

import Network.Wai (responseLBS, Application, requestMethod, requestBody, pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.Wai.Internal (ResponseReceived(..))
import Network.HTTP.Types.Header (hContentType)

import qualified Db
import Entities (Molecule(..))

entryPoint :: IO ()
entryPoint = do
    let port = 18080
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req resp
    | requestMethod req == (C8.pack "POST")
    = doPost req resp
    | otherwise
    = doGet req resp

doPost req resp = do
    c <- requestBody req
    writeFile "state.txt" (C8.unpack c)
    resp $ responseLBS
        status200
        []
        (LC8.pack "ya post\n")

doGet req resp =
    let path = filter (/= "") (map T.unpack (pathInfo req))
    in case path of
        ["molecule"] -> respondFun resp doGetAllMolecules
        ["molecule", strId] -> respondFun resp $ doGetMolecule strId
        _ -> respond resp (Just (show path))

respondFun resp fun = do
    res <- fun
    respond resp $ res

respond resp maybeRes = do
    let (stat, text) = case maybeRes of
                Nothing -> (status404, "not found :(")
                Just v -> (status200, v)
    resp $ responseLBS stat [] (LC8.pack $ text ++ "\n")
    return ResponseReceived

doGetMolecule :: String -> IO (Maybe String)
doGetMolecule strId = do
    m <- Db.fetchMolecule (read strId :: Int)
    case m of
        Just v -> return (Just (show v))
        Nothing -> return Nothing

doGetAllMolecules :: IO (Maybe String)
doGetAllMolecules = do
    ms <- Db.fetchAllMolecules
    return $ Just (show ms)

