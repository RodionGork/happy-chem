module Entities.Molecule (
    Molecule(..),
    fromStrings
    ) where

import qualified Data.List as List
import Data.Typeable (Typeable)

data Molecule = Molecule {id :: Int, iupacName :: String, smiles :: String}
    deriving (Eq, Typeable)

instance Show Molecule where
    show Molecule {Entities.Molecule.id = pcid, iupacName = name, smiles = smiles} =
        "[Molecule: " ++ name ++ ", #" ++ show pcid ++ ", " ++ smiles ++ "]"

fromStrings :: String -> String -> Molecule
fromStrings strId smilesAndName =
    let (smiles, (_:name)) = List.break (== ' ') smilesAndName
        pcid = read strId :: Int
    in Molecule {Entities.Molecule.id = pcid, iupacName = name, smiles = smiles}

