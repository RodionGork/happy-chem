module Entities.Catalyst (
    Catalyst(..),
    fromStrings
    ) where

import qualified Data.List as List
import Data.Maybe (Maybe, fromMaybe)
import Data.Typeable (Typeable)

data Catalyst = Catalyst {id :: Int, name :: Maybe String, smiles :: String}
    deriving (Eq, Typeable)

instance Show Catalyst where
    show Catalyst {Entities.Catalyst.id = pcId, name = name, smiles = smiles} =
        let nameStr = fromMaybe "?" name
        in "[Catalyst: " ++ nameStr ++ ", #" ++ show pcId ++ ", " ++ smiles ++ "]"

fromStrings :: String -> String -> Catalyst
fromStrings strId smilesAndName =
    let (smiles, namePart) = List.break (== ' ') smilesAndName
        pcid = read strId :: Int
        name = if namePart == ""
                    then Nothing
                    else Just $ drop 1 namePart
    in Catalyst {Entities.Catalyst.id = pcid, name = name, smiles = smiles}

