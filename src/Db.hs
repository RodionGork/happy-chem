module Db
    (
    fetchMolecule,
    fetchAllMolecules,
    storeMolecule,
    test 
    ) where

import qualified Control.Exception as Exc
import qualified Database.Bolt as Hb
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (Maybe, listToMaybe)
import qualified Data.Text as T

import Entities (Molecule(..))

toNode :: Monad m => Hb.Record -> m Hb.Node
toNode record = record `Hb.at` (T.pack "n") >>= Hb.exact

moleculeFromNode (Hb.Node {Hb.labels = labels, Hb.nodeProps = props}) =
    let name = textFromValue $ Map.lookup (T.pack "iupacName") props
        smiles = textFromValue $ Map.lookup (T.pack "smiles") props
        pcId = intFromValue $ Map.lookup (T.pack "id") props
    in Molecule {Entities.id = pcId, iupacName = name, smiles = smiles}

textFromValue :: Maybe Hb.Value -> T.Text
textFromValue (Just (Hb.T val)) = val
textFromValue _ = T.pack ""

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

fetchMolecule :: Int -> IO (Maybe Molecule)
fetchMolecule pcid = do
    records <- wrapInConnection $ Hb.queryP
        (T.pack "MATCH (n:molecule) where n.id={pcid} RETURN n")
        (Map.fromList [((T.pack "pcid"), Hb.I pcid)])
    nodes <- sequence $ Prelude.map toNode records
    return $ listToMaybe $ Prelude.map moleculeFromNode nodes

fetchAllMolecules :: IO [Molecule]
fetchAllMolecules = do
    records <- wrapInConnection $ Hb.query (T.pack "MATCH (n:molecule) RETURN n")
    nodes <- sequence $ Prelude.map toNode records
    return $ Prelude.map moleculeFromNode nodes

storeMoleculeUnsafe pcid name smiles = do
    k <- wrapInConnection $ Hb.queryP
        (T.pack "CREATE (:molecule {id:{i}, iupacName:{n}, smiles:{s}})")
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

-- not used now, just don't forget
initDb :: IO Bool
initDb = do
    records <- wrapInConnection $ Hb.query
        (T.pack "CREATE CONSTRAINT ON (m:molecule) ASSERT m.id IS UNIQUE")
    return True

test = do
    m <- fetchMolecule 1
    putStrLn $ show m

