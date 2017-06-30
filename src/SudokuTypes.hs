module SudokuTypes where

import Data.Char (digitToInt)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List (tails)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Printf
import Data.Maybe

type Idx = (Int, Int)

cells :: [Idx]
cells = [(i, j) | i <- [1 .. 9], j <- [1 .. 9]]

rows :: [[Idx]]
rows = [[(i, j) | j <- [1 .. 9]] | i <- [1 .. 9]]

rowOf :: (Int, t) -> [Idx]
rowOf (i, _) = rows !! (i - 1)

cols :: [[Idx]]
cols = [[(i, j) | i <- [1 .. 9]] | j <- [1 .. 9]]

colOf :: (t, Int) -> [Idx]
colOf (_, j) = cols !! (j - 1)

boxes :: [[Idx]]
boxes = map square $ (,) <$> [1, 4, 7] <*> [1, 4, 7]
  where
    square (i, j) = (,) <$> [i .. i + 2] <*> [j .. j + 2]

boxOf :: Idx -> [Idx]
boxOf ij = boxMap Map.! ij
  where
    boxMap =
        Map.fromList . concatMap (\box -> map (\ij -> (ij, box)) box) $ boxes

data Cell
    = Unsolved !IntSet
    | Solved !Int
    | Invalid
    deriving (Eq)

instance Show Cell where
    show (Unsolved s) = concatMap show (IntSet.toList s)
    show (Solved n) = show n
    show Invalid = "!"

cellIsUnsolved :: Cell -> Bool
cellIsUnsolved (Unsolved _) = True
cellIsUnsolved _ = False

cellIsSolved :: Cell -> Bool
cellIsSolved (Solved _) = True
cellIsSolved _ = False

cellIsInvalid :: Cell -> Bool
cellIsInvalid Invalid = True
cellIsInvalid _ = False

cellSize :: Cell -> Int
cellSize Invalid = 0
cellSize (Solved _) = 1
cellSize (Unsolved s) = IntSet.size s

cellChoices :: Cell -> [IntSet.Key]
cellChoices (Unsolved s) = IntSet.toList s
cellChoices _ = []

unsolvedCellSet :: Cell -> IntSet
unsolvedCellSet (Unsolved s) = s

defaultCellSet :: Cell
defaultCellSet = Unsolved (IntSet.fromList [1 .. 9])

deleteCellValues :: IntSet -> Cell -> Cell
deleteCellValues values (Unsolved s) =
    let s' = s IntSet.\\ values
    in case IntSet.size s' of
           0 -> Invalid
           1 -> (Solved . head . IntSet.toList) s'
           _ -> Unsolved s'
deleteCellValues _ cell = cell

newtype Board =
    Board (Map Idx Cell)
    deriving (Eq)

boardGetCell :: Board -> Idx -> Cell
boardGetCell (Board cs) ij = cs Map.! ij

instance Show Board where
    show (Board board) =
        concat $ mapGroup 9 (\xs -> concat xs ++ "\n") formattedCells
      where
        cellText = map show (Map.elems board)
        cellWidth = (maximum . map length) cellText + 1
        formattedCells = map (pad cellWidth) cellText
        pad n x = printf "%*s" n x
        mapGroup _ _ [] = []
        mapGroup n f xs = f (take n xs) : mapGroup n f (drop n xs)

boardIsSolved :: Board -> Bool
boardIsSolved b@(Board cs) = all cellIsSolved (Map.elems cs) && (isJust . verify) b

boardIsInvalid :: Board -> Bool
boardIsInvalid b@(Board cs) = any cellIsInvalid (Map.elems cs) || (isNothing . verify) b

boardSetCellValue :: Board -> Idx -> Int -> Board
boardSetCellValue board ij n = boardSetCell board ij (Solved n)

boardSetCell :: Board -> Idx -> Cell -> Board
boardSetCell (Board cs) ij cell = Board $ Map.insert ij cell cs

boardCells :: Board -> [(Idx, Cell)]
boardCells (Board cs) = Map.toAscList cs

parseBoard :: String -> Maybe Board
parseBoard puzzle
    | length cellSets == length cells = verify . Board . Map.fromList $ zip cells cellSets
    | otherwise = Nothing
  where
    cellSets = map convertNumber puzzle
    convertNumber n
        | n `elem` ['1' .. '9'] = Solved (digitToInt n)
        | otherwise = defaultCellSet

verify :: Board -> Maybe Board
verify board = if all verifyRange (rows ++ cols ++ boxes) then Just board else Nothing
  where
    verifyRange cs =
        let solvedCells =
                filter cellIsSolved $
                foldr (\a xs -> boardGetCell board a : xs) [] cs
        in length solvedCells == (length . List.nub) solvedCells
