module WebServer
    ( start
    ) where

import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP.Types
import Network.Wai.Internal (ResponseReceived(..))
import qualified Network.HTTP.Types.Header as HTTP.Header

import qualified Db
import Entities (Molecule(..))

start :: IO ()
start = do
    let port = 18080
    putStrLn $ "Listening on port " ++ show port
    Warp.run port app

app :: Wai.Application
app req resp
    | Wai.requestMethod req == (C8.pack "POST")
    = doPost req resp
    | otherwise
    = doGet req resp

doPost req resp = do
    body <- Wai.requestBody req
    case parsePath req of
        ["molecule", strId] -> respondFun resp $ doPostMolecule strId body

doGet req resp =
    case parsePath req of
        [] -> respondFile resp "index.html"
        ["static", fileName] -> respondFile resp fileName
        ["molecule"] -> respondFun resp doGetAllMolecules
        ["molecule", strId] -> respondFun resp $ doGetMolecule strId
        _ -> respond resp Nothing

parsePath req =
    filter (/= "") (map T.unpack (Wai.pathInfo req))

respondFun resp fun = do
    res <- fun
    respond resp $ res

respond resp maybeRes = do
    let (status, text) = case maybeRes of
                Nothing -> (HTTP.Types.notFound404, "not found :(")
                Just v -> (HTTP.Types.ok200, v)
        headers = [(HTTP.Header.hContentType, C8.pack "text/plain")]
    resp $ Wai.responseLBS status headers (LC8.pack $ text ++ "\n")
    return ResponseReceived

respondFile resp fileName = do
    let headers = [(HTTP.Header.hContentType, C8.pack $ staticContentType fileName)]
    resp $ Wai.responseFile HTTP.Types.ok200 headers ("static/" ++ fileName) Nothing

staticContentType fileName =
    case T.unpack $ T.takeWhileEnd (/= '.') (T.pack fileName) of
        "html" -> "text/html"
        "js" -> "application/javascript"
        "css" -> "text/css"
        _ -> "text/plain"

doGetMolecule :: String -> IO (Maybe String)
doGetMolecule strId = do
    m <- Db.fetchMolecule (read strId :: Int)
    case m of
        Just v -> return (Just (show v))
        Nothing -> return Nothing

doGetAllMolecules :: IO (Maybe String)
doGetAllMolecules = do
    ms <- Db.fetchAllMolecules
    let res = Prelude.map show ms
    return $ Just (List.intercalate "; " res)

doPostMolecule :: String -> C8.ByteString -> IO (Maybe String)
doPostMolecule strId body = do
    let (smiles, name) = T.breakOn (T.pack " ") (T.pack $ C8.unpack body)
        pcid = (read strId :: Int)
    success <- Db.storeMolecule pcid (T.strip name) smiles
    return $ Just (if success then "ok" else "error")

