module Entities.Catalyst (
    Catalyst(..)
    ) where

import Data.Maybe (Maybe, fromMaybe)

data Catalyst = Catalyst {id :: Int, name :: Maybe String, smiles :: String}
    deriving (Eq)

instance Show Catalyst where
    show Catalyst {Entities.Catalyst.id = pcId, name = name, smiles = smiles} =
        let nameStr = fromMaybe "?" name
        in "[Catalyst: " ++ nameStr ++ ", #" ++ (show pcId) ++ ", " ++ smiles ++ "]"

