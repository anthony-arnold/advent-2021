module SPFA where
import qualified Data.Set as Set

shortest next end start = search mempty (Set.singleton start)
  where search visited remaining = case Set.minView remaining of
          Nothing -> Nothing
          Just ((dist, v), without)
            | v == end -> Just (dist, v)
            | Set.member v visited -> search visited without
            | otherwise -> search visitedWith adj
            where visitedWith = Set.insert v visited
                  adj = foldr Set.insert without $ next (dist, v)
