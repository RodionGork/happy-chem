module Entities.Molecule (
    Molecule(..)
    ) where

data Molecule = Molecule {id :: Int, iupacName :: String, smiles :: String}
    deriving (Eq)

instance Show Molecule where
    show Molecule {Entities.Molecule.id = pcId, iupacName = name, smiles = smiles} =
        "[Molecule: " ++ name ++ ", #" ++ (show pcId) ++ ", " ++ smiles ++ "]"

