module Db
    (
    fetchMolecule,
    fetchAllMolecules,
    fetchCatalyst,
    fetchAllCatalysts,
    fetchReaction,
    fetchAllReactions,
    reactionIngredients,
    storeEntity,
    storeReagent,
    storeProduct,
    storeAccelerate,
    shortestPath,
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
import qualified Relations as Relations

class Persistent a where
    fromNode :: Hb.Node -> a
    toParamMap :: a -> Map.Map T.Text Hb.Value

toNode :: Monad m => Hb.Record -> m Hb.Node
toNode record = record `Hb.at` T.pack "n" >>= Hb.exact

toPath :: Monad m => Hb.Record -> m Hb.Path
toPath record = record `Hb.at` T.pack "p" >>= Hb.exact

toRel :: Monad m => Hb.Record -> m Hb.Relationship
toRel record = record `Hb.at` T.pack "x" >>= Hb.exact

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
        let name = strFromValue $ Map.lookup (T.pack "name") props
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

fetchReaction :: Int -> IO (Maybe Rct.Reaction)
fetchReaction pcid =
    fetchEntity "Reaction" pcid

fetchAllReactions :: IO [Rct.Reaction]
fetchAllReactions =
    fetchAllEntities "Reaction"

storeReagent str = do
    let params = Relations.reagentToParamMap str
        query = "MATCH (m:Molecule),(r:Reaction) WHERE m.id={mol} and r.id={rct} "
            ++ "CREATE (m)-[:REAGENT_IN {amount:{amount}}]->(r)"
    executeCreation query params

storeProduct str = do
    let params = Relations.productToParamMap str
        query = "MATCH (r:Reaction),(m:Molecule) WHERE r.id={rct} and m.id={mol} "
            ++ "CREATE (r)-[:PRODUCT_FROM {amount:{amount}}]->(m)"
    executeCreation query params

storeAccelerate str = do
    let params = Relations.accelerateToParamMap str
        query = "MATCH (c:Catalyst),(r:Reaction) WHERE r.id={rct} and c.id={cat} "
            ++ "CREATE (c)-[:ACCELERATE {temperature:{tmp}, pressure:{prs}}]->(r)"
    executeCreation query params

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
    executeCreation query params

executeCreation query params = do
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

shortestPath :: Int -> Int -> IO (Maybe (Mol.Molecule, [(Rct.Reaction, Mol.Molecule)]))
shortestPath srcId dstId = do
    let q = "MATCH (src:Molecule {id:{s}}),(dst:Molecule {id:{d}}), p=shortestPath((src)-[*..7]->(dst)) RETURN p"
        p = Map.fromList [(T.pack "s", Hb.I srcId), (T.pack "d", Hb.I dstId)]
    records <- wrapInConnection $ Hb.queryP (T.pack q) p
    case records of
        [] ->
            return Nothing
        (record:_) -> do
            path <- toPath record
            return $ Just $ pathToList path

pathToList :: Hb.Path -> (Mol.Molecule, [(Rct.Reaction, Mol.Molecule)])
pathToList Hb.Path {Hb.pathNodes = (src:nodes)} =
    (fromNode src, pathNodesToList nodes)

pathNodesToList :: [Hb.Node] -> [(Rct.Reaction, Mol.Molecule)]
pathNodesToList [] =
    []
pathNodesToList (rct:mol:tail) =
    [(fromNode rct, fromNode mol)] ++ (pathNodesToList tail)

reactionIngredients rid rel = do
    -- failed to find a way to match rel type with parameter :(
    let q = "MATCH (r:Reaction {id:{rid}})-[x:" ++ rel ++ "]-(n) RETURN x,n"
        p = Map.fromList [(T.pack "rid", Hb.I rid)]
    records <- wrapInConnection $ Hb.queryP (T.pack q) p
    res <- Prelude.mapM (\r -> do
        n <- toNode r
        let m = fromNode n
        s <- toRel r
        let Hb.Relationship {Hb.relProps = props} = s
        return (m :: Mol.Molecule, props)) records
    return res

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

