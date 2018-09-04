module Relations (
    reagentToParamMap,
    productToParamMap,
    accelerateToParamMap
    ) where

import qualified Database.Bolt as Hb
import qualified Data.Map as Map
import qualified Data.Text as T

reagentToParamMap str =
    let [reactionId, moleculeId, amount] = words str
    in Map.fromList [
        (T.pack "rct", Hb.I (read reactionId :: Int)),
        (T.pack "mol", Hb.I (read moleculeId :: Int)),
        (T.pack "amount", Hb.F (read amount :: Double))]

productToParamMap str =
    let [moleculeId, reactionId, amount] = words str
    in Map.fromList [
        (T.pack "mol", Hb.I (read moleculeId :: Int)),
        (T.pack "rct", Hb.I (read reactionId :: Int)),
        (T.pack "amount", Hb.F (read amount :: Double))]

accelerateToParamMap str =
    let [reactionId, catalystId, temperature, pressure] = words str
    in Map.fromList [
        (T.pack "rct", Hb.I (read reactionId :: Int)),
        (T.pack "cat", Hb.I (read catalystId :: Int)),
        (T.pack "tmp", Hb.F (read temperature :: Double)),
        (T.pack "prs", Hb.F (read pressure :: Double))]

