module Entities (
    Molecule(..)
    ) where

import qualified Data.Text as T

data Molecule = Molecule {id :: Int, iupacName :: T.Text, smiles :: T.Text}
    deriving (Show, Eq)

