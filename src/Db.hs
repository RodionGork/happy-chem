module Db
    (
    fetchMolecule,
    fetchAllMolecules,
    storeMolecule,
    fetchCatalyst,
    test 
    ) where

import qualified Control.Exception as Exc
import qualified Database.Bolt as Hb
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (Maybe, listToMaybe, maybe)
import qualified Data.Text as T

import qualified Entities.Molecule as Mol
import qualified Entities.Catalyst as Catl

class Persistent a where
    fromNode :: Hb.Node -> a

toNode :: Monad m => Hb.Record -> m Hb.Node
toNode record = record `Hb.at` (T.pack "n") >>= Hb.exact

instance Persistent Mol.Molecule where
    fromNode (Hb.Node {Hb.labels = labels, Hb.nodeProps = props}) =
        let name = strFromValue $ Map.lookup (T.pack "iupacName") props
            smiles = strFromValue $ Map.lookup (T.pack "smiles") props
            pcId = intFromValue $ Map.lookup (T.pack "id") props
        in Mol.Molecule {Mol.id = pcId, Mol.iupacName = name, Mol.smiles = smiles}

instance Persistent Catl.Catalyst where
    fromNode (Hb.Node {Hb.labels = labels, Hb.nodeProps = props}) =
        let name = maybeStrFromValue $ Map.lookup (T.pack "name") props
            smiles = strFromValue $ Map.lookup (T.pack "smiles") props
            pcId = intFromValue $ Map.lookup (T.pack "id") props
        in Catl.Catalyst {Catl.id = pcId, Catl.name = name, Catl.smiles = smiles}

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
        Hb.user = (T.pack "neo4j"),
        Hb.password = (T.pack "j4oen")}
    records <- Hb.run pipe query
    Hb.close pipe
    return records

catchAny :: IO a -> (Exc.SomeException -> IO a) -> IO a
catchAny = Exc.catch

fetchMolecule :: Int -> IO (Maybe Mol.Molecule)
fetchMolecule pcid = do
    records <- wrapInConnection $ Hb.queryP
        (T.pack "MATCH (n:Molecule) where n.id={pcid} RETURN n")
        (Map.fromList [((T.pack "pcid"), Hb.I pcid)])
    nodes <- sequence $ Prelude.map toNode records
    return $ listToMaybe $ Prelude.map fromNode nodes

fetchAllMolecules :: IO [Mol.Molecule]
fetchAllMolecules = do
    records <- wrapInConnection $ Hb.query (T.pack "MATCH (n:Molecule) RETURN n")
    nodes <- sequence $ Prelude.map toNode records
    return $ Prelude.map fromNode nodes

storeMoleculeUnsafe pcid name smiles = do
    k <- wrapInConnection $ Hb.queryP
        (T.pack "CREATE (:Molecule {id:{i}, iupacName:{n}, smiles:{s}})")
        (Map.fromList [
            ((T.pack "i"), Hb.I pcid),
            ((T.pack "n"), Hb.T name),
            ((T.pack "s"), Hb.T smiles)])
    return True

storeMolecule :: Int -> T.Text -> T.Text -> IO Bool
storeMolecule pcid name smiles = do
    result <- catchAny (storeMoleculeUnsafe pcid name smiles)
        (\e -> return False)
    return result

fetchCatalyst :: Int -> IO (Maybe Catl.Catalyst)
fetchCatalyst pcid = do
    records <- wrapInConnection $ Hb.queryP
        (T.pack "MATCH (n:Catalyst) where n.id={pcid} RETURN n")
        (Map.fromList [((T.pack "pcid"), Hb.I pcid)])
    nodes <- sequence $ Prelude.map toNode records
    return $ listToMaybe $ Prelude.map fromNode nodes

-- not used now, just don't forget
initDb :: IO Bool
initDb = do
    records <- wrapInConnection $ Hb.query
        (T.pack "CREATE CONSTRAINT ON (m:Molecule) ASSERT m.id IS UNIQUE")
    return True

test = do
    m <- fetchMolecule 1
    putStrLn $ show m

