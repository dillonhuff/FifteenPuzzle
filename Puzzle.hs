module Puzzle() where

import Data.List as L
import Data.Map as M
import System.Random

type Tile = Int
type Location = (Int, Int)

data Puzzle = Puzzle (M.Map Location Tile) Location

instance Show Puzzle where
  show = showPuzzle

showPuzzle :: Puzzle -> String
showPuzzle (Puzzle m _) = showTiles $ L.sortBy compareLocations $ M.toList m

compareLocations :: (Location, Tile) -> (Location, Tile) -> Ordering
compareLocations (l1, t1) (l2, t2) = compare l1 l2

showTiles :: [(Location, Tile)] -> String
showTiles [] = ""
showTiles locs = (L.concat $ L.map showTile (take 4 locs)) ++ "\n" ++ (showTiles (drop 4 locs))

showTile :: (Location, Tile) -> String
showTile (_, t) = case t < 10 of
  True -> " " ++ show t ++ " "
  False -> case t == 16 of
    True -> "__ "
    False -> show t ++ " "

tileAt :: Puzzle -> Location -> Tile
tileAt (Puzzle m _) l = case M.lookup l m of
  Just t -> t
  Nothing -> error $ show l ++ " is not a valid location"

locations = [(x, y) | x <- [1..4], y <- [1..4]]
tiles = [1..16]
blank = 16

newPuzzle :: Puzzle
newPuzzle = Puzzle (M.fromList (L.zip locations tiles)) (4, 4)

blankSpaceLocation :: Puzzle -> Location
blankSpaceLocation (Puzzle _ l) = l

adjacent :: Location -> [Location]
adjacent (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

data Move = Move Location Location
            deriving (Eq, Ord, Show)

start :: Move -> Location
start (Move s _) = s

end :: Move -> Location
end (Move _ e) = e

legalMoves :: Puzzle -> [Move]
legalMoves p = L.map (Move blankLoc) moveEnds
  where
    blankLoc = blankSpaceLocation p
    moveEnds = L.intersect locations (adjacent blankLoc)

doMove :: Puzzle -> Move -> Puzzle
doMove p@(Puzzle m l) (Move start end) = Puzzle (M.insert start (tileAt p end) (M.insert end blank m)) end

shufflePuzzle :: (RandomGen t) => t -> Int -> Puzzle -> Puzzle
shufflePuzzle gen numMoves puzzle = fst $ recShufflePuzzle gen numMoves puzzle

recShufflePuzzle :: (RandomGen t) => t -> Int -> Puzzle -> (Puzzle, t)
recShufflePuzzle gen 0 p = (p, gen)
recShufflePuzzle gen n p = recShufflePuzzle nextGen (n-1) movedPuzzle
  where
    possibleMoves = legalMoves p
    nextMoveAndGen = randomR (0, (length possibleMoves) - 1) gen
    nextGen = snd nextMoveAndGen
    moveInd = fst nextMoveAndGen
    move = possibleMoves !! moveInd
    movedPuzzle = doMove p move
