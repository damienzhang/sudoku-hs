module Main where

import Control.Monad.State
import Data.Foldable
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.List as List
import Data.List (tails)
import qualified Data.Set as Set
import Data.Set (Set, (\\))
import SudokuTypes

data PreSet = PreSet
  { psNumbers :: !IntSet
  , psCells :: !(Set Idx)
  }

instance Show PreSet where
  show ps =
    "{Numbers=" ++
    (show . IntSet.toList . psNumbers) ps ++
    ", cells=" ++ (show . Set.toList . psCells) ps ++ "}"

type BoardState a = StateT Board Maybe a -- Board -> Maybe (a, Board)

readCell :: Idx -> BoardState Cell
readCell ij = StateT $ \board -> Just (boardGetCell board ij, board)

writeCell :: Idx -> Maybe Cell -> BoardState ()
writeCell ij cell =
  StateT $ \board -> do
    c <- cell
    let newBoard = boardSetCell board ij c
    case newBoard of
      Nothing -> Nothing
      Just b -> Just ((), b)

crossOut
  :: Foldable f
  => f Idx -> IntSet -> BoardState ()
crossOut ijs values =
  forM_ ijs $ \ij ->
    readCell ij >>= return . deleteCellValues values >>= writeCell ij
    -- cell <-
    -- writeCell ij ()

markup :: Board -> Maybe Board
markup board =
  flip execStateT board $
  forM_ (boardCells board) $ \(ij, cell) -> do
    when (cellIsSolved cell) $ do
      let Solved n = cell
      let peers = filter (/= ij) $ colOf ij ++ rowOf ij ++ boxOf ij
      crossOut peers (IntSet.singleton n)

applyPreSets :: Board -> Maybe Board
applyPreSets board =
  flip execStateT board $ do
    go rows
    go cols
    go boxes
  where
    go ranges =
      forM_ ranges $ \ijs -> do
        b <- get
        forM_ (preSets b ijs) $ \ps -> do
          let otherCells = Set.fromList ijs \\ psCells ps
          crossOut otherCells (psNumbers ps)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y : ys | y:xs' <- tails xs, ys <- combinations (n - 1) xs']

removeAny
  :: Eq a
  => [a] -> [[a]] -> [[a]]
removeAny xs = filter (\ys -> not . or $ (==) <$> xs <*> ys)

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
                  (PreSet {psNumbers = nums, psCells = Set.fromList x} : acc)
           else go xs acc

preSets :: Board -> [Idx] -> [PreSet]
preSets board ijs = ps 2 ++ ps 3 ++ ps 4
  where
    ps n = findPreSets board n ijs

chooseUnsolvedCell :: Board -> (Idx, Cell)
chooseUnsolvedCell board =
  List.minimumBy (\(_, x) (_, y) -> compare (cellSize x) (cellSize y)) $
  filter (cellIsUnsolved . snd) . boardCells $ board

simplify :: Board -> Maybe Board
simplify board = go (Just board) Nothing
  where
    go a b
      | a == b = a
      | otherwise = go (a >>= markup >>= applyPreSets) a

solve :: Board -> Maybe Board
solve board = do
  simplifiedBoard <- verify =<< simplify board
  if boardIsSolved simplifiedBoard
    then Just simplifiedBoard
    else do
      let (ij, cell) = chooseUnsolvedCell simplifiedBoard
      let solvedBoards =
            map
              (solve <=< boardSetCellValue simplifiedBoard ij)
              (cellValues cell)
      firstValid solvedBoards
  where
    firstValid = asum

main :: IO ()
main = do
  line <- getLine
  let solution = join (solve <$> parseBoard line)
  case solution of
    Just board -> print board
    Nothing -> putStrLn "Invalid puzzle"
  main
