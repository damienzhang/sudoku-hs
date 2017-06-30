module Main where

import Control.Monad.State
import Data.Foldable
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.List as List
import Data.List (tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import SudokuTypes
import qualified Data.Text as T
import Debug.Trace

sample = ".....6....59.....82....8....45........3........6..3.54...325..6.................."

-- sample =
--        "_395_______18_9_7_____1_9_41__4____3___________7___86___67_82___1__9___5_____1__8"

-- | Preemptive Set
data PreSet = PreSet
    { psNumbers :: !IntSet
    , psCells :: !(Set Idx)
    }

instance Show PreSet where
    show ps =
        "{Numbers=" ++
        (show . IntSet.toList . psNumbers) ps ++
        ", cells=" ++ (show . Set.toList . psCells) ps ++ "}"

type BoardState = State Board

writeCell :: Idx -> Cell -> BoardState ()
writeCell ij cell = state $ \board -> ((), boardSetCell board ij cell)

readCell :: Idx -> BoardState Cell
readCell ij = state $ \board -> (boardGetCell board ij, board)

markup :: Board -> Board
markup = execState doMarkup
  where
    doMarkup = do
        board <- get :: (BoardState Board)
        forM_ (boardCells board) $ \(ij, cell) ->
            case cell of
                Solved v -> do
                    removeValueFromCells v (filter (/= ij) . colOf $ ij)
                    removeValueFromCells v (filter (/= ij) . boxOf $ ij)
                    removeValueFromCells v (filter (/= ij) . rowOf $ ij)
                _ -> return ()
    removeValueFromCells v =
        mapM_
            (\ij ->
                 readCell ij >>= return . deleteCellValues (IntSet.singleton v) >>=
                 writeCell ij)


combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y : ys | y:xs' <- tails xs, ys <- combinations (n - 1) xs']

mapBoardValues f (Board b) = map f (Map.elems b)

removeAny
    :: Eq a
    => [a] -> [[a]] -> [[a]]
removeAny xs = filter (\ys -> not . or $ (==) <$> xs <*> ys)

sampleBoard = (markup . fromJust . parseBoard) sample

findPreSets :: Board -> Int -> [Idx] -> [PreSet]
findPreSets board n ijs = go (combinations n unsolvedCells) []
  where
    unsolvedCells = filter (cellIsUnsolved . boardGetCell board) ijs
    go [] acc = acc
    go (x:xs) acc =
        let nums =
                foldr
                    (IntSet.union . unsolvedCellSet . boardGetCell board)
                    IntSet.empty
                    x
        in if IntSet.size nums == n
               then go
                        (removeAny x xs)
                        (PreSet {psNumbers = nums, psCells = Set.fromList x} :
                         acc)
               else go xs acc

preSets board ijs = ps 2 ++ ps 3 ++ ps 4
  where
    ps n = findPreSets board n ijs

applyPreSets :: Board -> Board
applyPreSets = execState go
  where
    go = do
        crossOut rows
        crossOut cols
        crossOut boxes
    crossOut ranges =
        forM_ ranges $ \ijs -> do
            b <- get :: (BoardState Board)
            forM_ (preSets b ijs) $ \ps -> do
                let otherCells = Set.fromList ijs \\ psCells ps
                removeValuesFromCells (psNumbers ps) otherCells
    removeValuesFromCells v =
        mapM_
            (\ij -> readCell ij >>= return . deleteCellValues v >>= writeCell ij)

chooseSearchCell :: Board -> (Idx, Cell)
chooseSearchCell board =
    List.minimumBy (\(_, x) (_, y) -> compare (cellSize x) (cellSize y)) $
    filter (cellIsUnsolved . snd) . boardCells $ board

converge (x:xs) =
    if x == head xs
        then x
        else converge xs

solve :: Board -> Maybe Board
solve board
    | boardIsInvalid simplifiedBoard = Nothing
    | boardIsSolved simplifiedBoard = Just simplifiedBoard
    | otherwise = doSearch
  where
    simplifiedBoard = (converge . iterate (applyPreSets . markup)) board
    doSearch =
        let (ij, cell) = chooseSearchCell simplifiedBoard
            solvedBoards =
                map
                    (solve . boardSetCellValue simplifiedBoard ij)
                    (cellChoices cell)
        in asum solvedBoards -- first item in solvedBoards that is not Nothing


main :: IO ()
main = do
    line <- getLine
    let solution = join (solve <$> parseBoard line)
    case solution of
        Just board -> print board
        Nothing -> putStrLn "Y U DUM"
    main
