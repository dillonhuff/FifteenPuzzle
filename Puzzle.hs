module Puzzle() where

import Data.List as L
import Data.Map as M

type Tile = Int
type Location = (Int, Int)

data Puzzle = Puzzle (M.Map Location Tile)

instance Show Puzzle where
  show = showPuzzle

showPuzzle :: Puzzle -> String
showPuzzle (Puzzle m) = showTiles $ L.sortBy compareLocations $ M.toList m

-- Fill in later
compareLocations :: (Location, Tile) -> (Location, Tile) -> Ordering
compareLocations (r1, c1) (r2, c2) = case r1 > r2 of
  True -> GT
  False -> LT

showTiles :: [(Location, Tile)] -> String
showTiles [] = ""
showTiles locs = (L.concat $ L.map showTile (take 4 locs)) ++ "\n" ++ (showTiles (drop 4 locs))

showTile :: (Location, Tile) -> String
showTile (_, t) = case t < 10 of
  True -> " " ++ show t ++ " "
  False -> case t == 16 of
    True -> "__ "
    False -> show t ++ " "

newPuzzle :: Puzzle
newPuzzle = Puzzle $ M.fromList (L.zip [(x, y) | x <- [1..4], y <- [1..4]] [1..16])

tile :: Puzzle -> Location -> Tile
tile (Puzzle m) loc = case M.lookup loc m of
  Just t -> t
  Nothing -> error $ "No such location as " ++ (show loc)
