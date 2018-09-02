module WebServer
    ( start
    ) where

import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Typeable (Typeable)

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types as HTTP.Types
import Network.Wai.Internal (ResponseReceived(..))
import qualified Network.HTTP.Types.Header as HTTP.Header

import qualified Db
import qualified Entities.Molecule as Mol
import qualified Entities.Catalyst as Catl
import qualified Entities.Reaction as Rct

start :: IO ()
start = do
    let port = 18080
    putStrLn $ "Listening on port " ++ show port
    Warp.run port app

app :: Wai.Application
app req resp
    | Wai.requestMethod req == C8.pack "POST"
    = doPost req resp
    | otherwise
    = doGet req resp

doPost req resp = do
    body <- Wai.requestBody req
    case parsePath req of
        ["molecule", strId] -> respondFun resp $ doPostMolecule strId body
        ["catalyst", strId] -> respondFun resp $ doPostCatalyst strId body
        ["reaction", strId] -> respondFun resp $ doPostReaction strId body
        ["reagent_in"] -> respondFun resp $ doPostReagent body
        ["product_from"] -> respondFun resp $ doPostProduct body
        ["accelerate"] -> respondFun resp $ doPostAccelerate body

doGet req resp =
    case parsePath req of
        [] -> respondFile resp "index.html"
        ["static", fileName] -> respondFile resp fileName
        ["molecule"] -> respondFun resp doGetAllMolecules
        ["molecule", strId] -> respondFun resp $ doGetMolecule strId
        ["catalyst"] -> respondFun resp $ doGetAllCatalysts
        ["catalyst", strId] -> respondFun resp $ doGetCatalyst strId
        _ -> respond resp Nothing

parsePath req =
    filter (/= "") (map T.unpack (Wai.pathInfo req))

respondFun resp fun = do
    res <- fun
    respond resp res

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
doGetMolecule strId =
    doGetEntity Db.fetchMolecule strId

doGetAllMolecules :: IO (Maybe String)
doGetAllMolecules =
    doGetAllEntities Db.fetchAllMolecules

doPostMolecule :: String -> C8.ByteString -> IO (Maybe String)
doPostMolecule strId body =
    doPostEntity Mol.fromStrings strId body

doGetCatalyst :: String -> IO (Maybe String)
doGetCatalyst strId =
    doGetEntity Db.fetchCatalyst strId

doGetAllCatalysts :: IO (Maybe String)
doGetAllCatalysts =
    doGetAllEntities Db.fetchAllCatalysts

doPostCatalyst :: String -> C8.ByteString -> IO (Maybe String)
doPostCatalyst strId body =
    doPostEntity Catl.fromStrings strId body

doPostReaction :: String -> C8.ByteString -> IO (Maybe String)
doPostReaction strId body =
    doPostEntity Rct.fromStrings strId body

doPostReagent :: C8.ByteString -> IO (Maybe String)
doPostReagent body = do
    doStore $ Db.storeReagent $ C8.unpack body

doPostProduct :: C8.ByteString -> IO (Maybe String)
doPostProduct body = do
    doStore $ Db.storeProduct $ C8.unpack body

doPostAccelerate :: C8.ByteString -> IO (Maybe String)
doPostAccelerate body = do
    doStore $ Db.storeAccelerate $ C8.unpack body

doGetEntity :: (Show a) => (Int -> IO (Maybe a)) -> String -> IO (Maybe String)
doGetEntity fetchFunc strId = do
    m <- fetchFunc (read strId :: Int)
    case m of
        Just v -> return (Just (show v))
        Nothing -> return Nothing

doGetAllEntities :: (Show a) => IO [a] -> IO (Maybe String)
doGetAllEntities fetchFunc = do
    ms <- fetchFunc
    let res = Prelude.map show ms
    return $ Just (List.intercalate "; " res)

doPostEntity :: (Db.Persistent a, Typeable a) =>
    (p -> String -> a) -> p -> C8.ByteString -> IO (Maybe String)
doPostEntity func strId body = do
    let entity = func strId (C8.unpack body)
    doStore $ Db.storeEntity entity

doStore func = do
    success <- func
    return $ Just (if success then "ok" else "error")

