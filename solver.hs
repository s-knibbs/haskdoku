import qualified Data.Set as Set
import Data.Char
import Data.Array
import Data.Functor
import Control.Monad.State
import Control.Monad.Trans
import System.IO
import System.Environment 

-- Structure to hold the grid state
data Grid = Grid {
    solution :: Array Int Int,
    candidates :: Array Int (Set.Set Int)
} deriving (Show)

main = do
    filename <- head <$> getArgs
    withFile filename ReadMode $ \handle -> do
        problem <- replicateM size (hGetLine handle)
        let endState = execState (do
            gs <- get
            solveGrid $ initialSolved $ solution gs
            ) (initGrid problem)
        printSolution $ solution endState
        where
            initialSolved = filter ((>0) . snd) . assocs

-- Grid size
size :: Int
size = 9

-- Get a list column indexes related to the given index
colIdxs :: Int -> Set.Set Int
colIdxs n = Set.fromList [x * size + (n `mod` size) | x <- [0..(size - 1)]]

-- Get a list of row indexes related to the given index
rowIdxs :: Int -> Set.Set Int
rowIdxs n = Set.fromList [(div n size) * size + x | x <- [0..(size - 1)]]

-- Get a list of indexes for the 3x3 sector of the given index
sectorIdxs :: Int -> Set.Set Int
sectorIdxs n = Set.fromList $ concat $ map (sectorRow x y) [0..2]
    where
        y = ((div n size) `div` 3) * 3
        x = ((mod n size) `div` 3) * 3
        sectorRow x y off = [n + x + size * (y + off) | n <- [0..2]]

-- Initialise the grid given a list of lines in the problem file
initGrid :: [String] -> Grid
initGrid problem = Grid sol cand
    where
        -- Convert to a flattened list
        problemList = concat $ map (map digitToInt) problem
        bounds = (0, size ^ 2 - 1)
        sol = listArray bounds problemList
        cand = listArray bounds $ replicate (size^2) (Set.fromList [1..size])

solveGrid :: [(Int, Int)] -> State Grid [(Int, Int)]
solveGrid [] = return []
solveGrid curSolved = do
    nextSolved <- (mapM solveIdx curSolved) >>= solveRowColsSectors . concat
    -- Update the solution array in the grid
    gs <- get
    put $ gs { solution = (solution gs) // nextSolved }
    solveGrid nextSolved

-- Return true if value is available to be placed at idx
validIdx :: Array Int (Set.Set Int) -> Int -> Int -> Bool 
validIdx cand value idx = Set.member value (cand ! idx)

-- Solve by finding single candidates across rows, columns and sectors
solveRowColsSectors :: [(Int, Int)] -> State Grid [(Int, Int)]
solveRowColsSectors [] = do
    gs <- get
    let c = candidates gs
        validIdx' = validIdx c
        oneMatch pred = (==1) . Set.size . (Set.filter pred)
        f solved (idx, cellSet) = solved ++ (Set.foldl (f' idx) [] cellSet)
        f' idx' xs val
            | (oneMatch (validIdx' val) (colIdxs idx')) = (idx', val):xs
            | (oneMatch (validIdx' val) (rowIdxs idx')) = (idx', val):xs
            | (oneMatch (validIdx' val) (sectorIdxs idx')) = (idx', val):xs
            | otherwise = xs
    return $ foldl f [] $ assocs c
solveRowColsSectors xs = return xs

-- Solve by finding cells with only one available value
solveIdx :: (Int, Int) -> State Grid [(Int, Int)]
solveIdx (idx, value) = do
    gs <- get
    let idxSet = Set.delete idx $ Set.unions [(colIdxs idx), (rowIdxs idx), (sectorIdxs idx)]
        c = candidates gs
        -- Get cell sets with value removed
        newCandidates = map (\idx -> (idx, Set.delete value (c ! idx))) $ Set.toList idxSet
        -- Get all cell sets with a single element
        singleCandidates = filter ((==1) . Set.size . snd) newCandidates
        emptyCandidates = (idx, Set.empty) : (map (\x -> (fst x, Set.empty)) singleCandidates)
    put $ gs { candidates = c // newCandidates // emptyCandidates }
    -- Return a list of [(idx, solved_value)]
    return $ map (\(idx, val) -> (idx, head $ Set.elems val)) singleCandidates

printSolution solution = do
    let sliceBounds = [(size * i, size * i + size - 1) | i <- [0..(size - 1)]]
        arraySlice a bounds = elems $ ixmap bounds id a
    mapM_ (putStrLn . concat . map ((++" ") . show) . arraySlice solution) sliceBounds