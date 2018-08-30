module Db
    (
    fetchMolecule,
    test 
    ) where

import Database.Bolt
import Data.Default
import Data.Map
import Data.Maybe
import qualified Data.Text as T

toNode :: Monad m => Record -> m Node
toNode record = record `at` (T.pack "n") >>= exact

data Molecule = Molecule {id :: Int, iupacName :: T.Text, smiles :: T.Text}
    deriving (Show, Eq)

moleculeFromNode (Node {labels = labels, nodeProps = props}) =
    let name = textFromValue $ Data.Map.lookup (T.pack "iupacName") props
        smiles = textFromValue $ Data.Map.lookup (T.pack "smiles") props
        pcId = intFromValue $ Data.Map.lookup (T.pack "id") props
    in Molecule {Db.id = pcId, iupacName = name, smiles = smiles}

textFromValue :: Maybe Value -> T.Text
textFromValue (Just (T val)) = val
textFromValue _ = T.pack ""

intFromValue :: Maybe Value -> Int
intFromValue (Just (I val)) = val
intFromValue _ = 0

fetchMolecule :: Int -> IO Molecule
fetchMolecule pcid = do
    pipe <- connect $ def { host = "172.17.0.1", user = (T.pack "neo4j"), password = (T.pack "j4oen") }
    records <- run pipe $ queryP (T.pack "MATCH (n:molecule) where n.id={pcid} RETURN n") (fromList [((T.pack "pcid"), I pcid)])
    putStrLn $ show records
    node <- toNode (head records)
    close pipe
    return $ moleculeFromNode node

test = do
    m <- fetchMolecule 1
    putStrLn $ show m

