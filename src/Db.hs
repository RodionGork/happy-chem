module Db
    (
    fetchMolecule,
    fetchAllMolecules,
    test 
    ) where

import Database.Bolt
import Data.Default
import Data.Map
import Data.Maybe
import qualified Data.Text as T

import Entities (Molecule(..))

toNode :: Monad m => Record -> m Node
toNode record = record `at` (T.pack "n") >>= exact

moleculeFromNode (Node {labels = labels, nodeProps = props}) =
    let name = textFromValue $ Data.Map.lookup (T.pack "iupacName") props
        smiles = textFromValue $ Data.Map.lookup (T.pack "smiles") props
        pcId = intFromValue $ Data.Map.lookup (T.pack "id") props
    in Molecule {Entities.id = pcId, iupacName = name, smiles = smiles}

textFromValue :: Maybe Value -> T.Text
textFromValue (Just (T val)) = val
textFromValue _ = T.pack ""

intFromValue :: Maybe Value -> Int
intFromValue (Just (I val)) = val
intFromValue _ = 0

wrapInConnection query = do
    pipe <- connect $ def { host = "172.17.0.1", user = (T.pack "neo4j"), password = (T.pack "j4oen") }
    records <- run pipe query
    close pipe
    return records

fetchMolecule :: Int -> IO (Maybe Molecule)
fetchMolecule pcid = do
    records <- wrapInConnection $ queryP
        (T.pack "MATCH (n:molecule) where n.id={pcid} RETURN n")
        (fromList [((T.pack "pcid"), I pcid)])
    nodes <- sequence $ Prelude.map toNode records
    return $ listToMaybe $ Prelude.map moleculeFromNode nodes

fetchAllMolecules :: IO [Molecule]
fetchAllMolecules = do
    records <- wrapInConnection $ query (T.pack "MATCH (n:molecule) RETURN n")
    nodes <- sequence $ Prelude.map toNode records
    return $ Prelude.map moleculeFromNode nodes

test = do
    m <- fetchMolecule 1
    putStrLn $ show m

