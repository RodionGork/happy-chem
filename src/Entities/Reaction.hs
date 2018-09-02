module Entities.Reaction (
    Reaction(..),
    fromStrings
    ) where

import qualified Data.List as List
import Data.Typeable (Typeable)

data Reaction = Reaction {id :: Int, name :: String}
    deriving (Eq, Typeable)

instance Show Reaction where
    show Reaction {Entities.Reaction.id = _id, name = name} =
        "[Reaction#" ++ show _id ++ ": " ++ name ++ "]"

fromStrings :: String -> String -> Reaction
fromStrings strId name =
    let _id = read strId :: Int
    in Reaction {Entities.Reaction.id = _id, name = name}

