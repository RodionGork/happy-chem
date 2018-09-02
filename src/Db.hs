module Db
    (
    fetchMolecule,
    fetchAllMolecules,
    fetchCatalyst,
    fetchAllCatalysts,
    storeEntity,
    Persistent
    ) where

import qualified Control.Exception as Exc
import qualified Database.Bolt as Hb
import Data.Default (def)
import Data.Typeable (Typeable, typeOf)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (Maybe, listToMaybe, maybe)
import qualified Data.Text as T

import qualified Entities.Molecule as Mol
import qualified Entities.Catalyst as Catl
import qualified Entities.Reaction as Rct

class Persistent a where
    fromNode :: Hb.Node -> a
    toParamMap :: a -> Map.Map T.Text Hb.Value

toNode :: Monad m => Hb.Record -> m Hb.Node
toNode record = record `Hb.at` T.pack "n" >>= Hb.exact

instance Persistent Mol.Molecule where
    fromNode Hb.Node {Hb.labels = labels, Hb.nodeProps = props} =
        let name = strFromValue $ Map.lookup (T.pack "iupacName") props
            smiles = strFromValue $ Map.lookup (T.pack "smiles") props
            pcId = intFromValue $ Map.lookup (T.pack "id") props
        in Mol.Molecule {Mol.id = pcId, Mol.iupacName = name, Mol.smiles = smiles}
    toParamMap m =
        Map.fromList [
            (T.pack "id", Hb.I $ Mol.id m),
            (T.pack "iupacName", Hb.T $ T.pack $ Mol.iupacName m),
            (T.pack "smiles", Hb.T $ T.pack $ Mol.smiles m)]

instance Persistent Catl.Catalyst where
    fromNode Hb.Node {Hb.labels = labels, Hb.nodeProps = props} =
        let name = maybeStrFromValue $ Map.lookup (T.pack "name") props
            smiles = strFromValue $ Map.lookup (T.pack "smiles") props
            pcId = intFromValue $ Map.lookup (T.pack "id") props
        in Catl.Catalyst {Catl.id = pcId, Catl.name = name, Catl.smiles = smiles}
    toParamMap c =
        let map = Map.fromList [
                    (T.pack "id", Hb.I $ Catl.id c),
                    (T.pack "smiles", Hb.T $ T.pack $ Catl.smiles c)]
        in case Catl.name c of
            Just n -> Map.insert (T.pack "name") (Hb.T $ T.pack n) map
            Nothing -> map

instance Persistent Rct.Reaction where
    fromNode Hb.Node {Hb.labels = labels, Hb.nodeProps = props} =
        let name = strFromValue $ Map.lookup (T.pack "iupacName") props
            pcId = intFromValue $ Map.lookup (T.pack "id") props
        in Rct.Reaction {Rct.id = pcId, Rct.name = name}
    toParamMap m =
        Map.fromList [
            (T.pack "id", Hb.I $ Rct.id m),
            (T.pack "name", Hb.T $ T.pack $ Rct.name m)]

strFromValue :: Maybe Hb.Value -> String
strFromValue (Just (Hb.T val)) = T.unpack val
strFromValue _ = ""

maybeStrFromValue :: Maybe Hb.Value -> Maybe String
maybeStrFromValue (Just (Hb.T val)) = Just $ T.unpack val
maybeStrFromValue _ = Nothing

intFromValue :: Maybe Hb.Value -> Int
intFromValue (Just (Hb.I val)) = val
intFromValue _ = 0

wrapInConnection query = do
    pipe <- Hb.connect $ def {
        Hb.host = "172.17.0.1",
        Hb.user = T.pack "neo4j",
        Hb.password = T.pack "j4oen"}
    records <- Hb.run pipe query
    Hb.close pipe
    return records

catchAny :: IO a -> (Exc.SomeException -> IO a) -> IO a
catchAny = Exc.catch

fetchMolecule :: Int -> IO (Maybe Mol.Molecule)
fetchMolecule pcid =
    fetchEntity "Molecule" pcid

fetchAllMolecules :: IO [Mol.Molecule]
fetchAllMolecules =
    fetchAllEntities "Molecule"

fetchCatalyst :: Int -> IO (Maybe Catl.Catalyst)
fetchCatalyst pcid =
    fetchEntity "Catalyst" pcid

fetchAllCatalysts :: IO [Catl.Catalyst]
fetchAllCatalysts =
    fetchAllEntities "Catalyst"

fetchEntity :: (Persistent a) => String -> Int -> IO (Maybe a)
fetchEntity label pcid = do
    records <- wrapInConnection $ Hb.queryP
        (T.pack $ "MATCH (n:" ++ label ++ ") where n.id={pcid} RETURN n")
        (Map.fromList [(T.pack "pcid", Hb.I pcid)])
    nodes <- Prelude.mapM toNode records
    return $ listToMaybe $ Prelude.map fromNode nodes

fetchAllEntities :: (Persistent a) => String -> IO [a]
fetchAllEntities label = do
    records <- wrapInConnection $ Hb.query (T.pack $ "MATCH (n:" ++ label ++ ") RETURN n")
    nodes <- Prelude.mapM toNode records
    return $ Prelude.map fromNode nodes

storeEntity :: (Persistent a, Typeable a) => a -> IO Bool
storeEntity entity = do
    let params = toParamMap entity
        query = queryForCreate (show $ typeOf entity) params
    catchAny (storeEntityUnsafe query params)
        (\e -> do
            print e
            return False)

storeEntityUnsafe query params = do
    wrapInConnection $ Hb.queryP (T.pack query) params
    return True

queryForCreate label params =
    let f = (\k -> let s = T.unpack k in s ++ ":{" ++ s ++ "}")
        attribs = Prelude.map f $ Map.keys params
    in "CREATE (:" ++ label ++ " {" ++ (List.intercalate "," attribs) ++ "})"

-- not used now, just don't forget
initDb :: IO Bool
initDb = do
    wrapInConnection $ Hb.query
        (T.pack "CREATE CONSTRAINT ON (m:Molecule) ASSERT m.id IS UNIQUE")
    wrapInConnection $ Hb.query
        (T.pack "CREATE CONSTRAINT ON (c:Catalyst) ASSERT c.id IS UNIQUE")
    wrapInConnection $ Hb.query
        (T.pack "CREATE CONSTRAINT ON (r:Reaction) ASSERT r.id IS UNIQUE")
    return True

