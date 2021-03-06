module Logic.Sudoku where

import qualified Logic.GameLogic as GL
import Data.List (sort, find, permutations, intercalate, (\\), transpose,
                  intersect)
import Data.Maybe (fromMaybe, catMaybes, isNothing, isJust, fromJust)
import Control.Parallel.Strategies
import Debug.Trace(traceShow)
import Data.Monoid ((<>))
import Control.Monad (guard)
import Prelude

type R          = Bool
type Value      = Int                             -- A number in a square. 
type Move       = [Value]                         -- A row in the puzzle.
type Coordinate = Int
type Position   = (Coordinate, Coordinate) 
type Puzzle     = [(Coordinate, Coordinate, Value)]

{-
For the 4x4 puzzle:

     1   2   3   4 
    ——— ——— ——— ——— 
 1 |   | 2 |   | 1 |
    ——— ——— ——— ——— 
 2 | 1 |   |   |   |
    ——— ——— ——— ——— 
 3 |   |   |   | 4 |
    ——— ——— ——— ——— 
 4 | 4 |   | 3 |   |
    ——— ——— ——— ——— 
 -}

size = 4

-- The parallelism does have an effect, but the puzzle is too small to
-- benefit from it.
main :: IO ()
main = print "hello world"
        {-let matchClue' [_,2, _,1] = True-}
            {-matchClue' _          = False-}
            {-allPossibleStarts :: [[Move]]-}
            {-allPossibleStarts = -}
                {-map (:[]) $ filter matchClue' $ permutations [1..size]-}
            {--- Find optimal plays for all starting moves that match the top-}
            {--- row of the puzzle, using parallelism.-}
        {-let results :: [Move]-}
            {-results = (flip GL.find p) $ -}
                    {-parMap rdeepseq parOptimalPlay allPossibleStarts-}
        {-if not . p $ results-}
            {-then putStrLn "I couldn't solve this puzzle."-}
            {-else putStrLn . prettyPrint $ results-}

-- Change a set of rows of values to a set of tuples, including their
-- x and y positions (from bottom left of the grid to top right).
movesToPuzzle :: [Move] -> Puzzle
movesToPuzzle = zip3 xs ys . concat
    where coords = do
                   y <- [1..size]
                   x <- [1..size]
                   return (x, y)
          (xs, ys) = unzip coords

-- Filter is slow O(n), need better technique.
checkPuzzle :: Puzzle -> Bool
checkPuzzle xs = 
        all (== allNums) (map sort rows) &&
        all (== allNums) (map sort cols) &&
        all (== allNums) (map sort grids)
    where rows, cols :: [[Value]]
          rows = do -- Get every row in the puzzle 
              y <- [1..size]
              let ls = filter (\(_,y',_) -> y' == y) xs
              return $ map (\(_,_,n) -> n) ls
          cols = do -- Get every column in the puzzle 
              x <- [1..size]
              let ls = filter (\(x',_,_) -> x' == x) xs
              return $ map (\(_,_,n) -> n) ls
          grids :: [[Value]] -- Get a list of every grid's value 
          grids = getGrids size xs
          allNums = [1..size] -- The set of numbers to match. 
          -- Change a list of positions into a list of grids of values.
          getGrids :: Int -> Puzzle -> [[Value]]
          getGrids size xs = gridify $ cellValues xs
              -- Turn a list of values ordered by their places in each grid
              -- square into a list of lists, consisting of each grid square.
              where gridify :: [Value] -> [[Value]] 
                    gridify [] = []
                    gridify xs = take size xs : gridify (drop size xs)
                    -- Change a list of positions to a list of values, 
                    -- ordered by each grid square they appear in.
                    cellValues :: Puzzle -> [Value]
                    cellValues xs = do
                      let root = round $ sqrt (fromIntegral size :: Double)
                      y' <- [1..root]
                      x' <- [1..root]
                      x  <- [1..root]
                      y  <- [1..root]
                      let x'' = x + (pred x' * root)
                          y'' = y + (pred y' * root)
                      return $ getValue (x'', y'') xs

getValue :: (Coordinate, Coordinate) -> Puzzle -> Value
getValue _ []          = undefined
getValue (x, y) ps = 
    let (_,_,v) = fromMaybe undefined $ 
                    find (\(x',y',_) -> x == x' && y == y') ps
    in  v

valid :: Puzzle -> R
valid = checkPuzzle

-- Must also match the puzzle's inital clues.
p :: [Maybe Int] -> [Move] -> R
p clues ms = valid (movesToPuzzle ms) && matchClue clues ms

pPar :: [Maybe Int] -> [Move] -> [Move] -> R
pPar clues preceding ms = valid (movesToPuzzle ms') && matchClue clues ms'
    where ms' = preceding ++ ms

-- Take a list of clues and played moves, and check if the played moves
-- match the clues.
matchClue :: [Maybe Int] -> [Move] -> Bool
matchClue clues ms = foldl check True clues'
    where xs, ys :: [Int]
          (xs, ys) = unzip [(x, y) | y <- [1..size], x <- [1..size]]
          clues' :: [(Coordinate, Coordinate, Value)]
          clues'   = map (\(x,y,mz) -> (x,y,fromJust mz)) $ 
                        filter (\(_,_,mz) -> isJust mz) $ zip3 xs ys clues
          check :: Bool -> (Coordinate, Coordinate, Value) -> Bool
          check b (x,y,c) = b && getValue (x,y) (movesToPuzzle ms) == c
 
optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons (p [])

-- Each move (a row in the puzzle) is a permutation of [1..n] for n sized
-- puzzles. Also filtered the permutation to the only possible number 
-- sequences for faster results (otherwise, doesn't terminate).
epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate size epsilon
    where epsilon h = GL.find (possibilities `GL.setMinus` h)
          possibilities = permutations [1..size] 

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = replicate (size - length preceding) epsilon
    where epsilon h = GL.find (possibilities `GL.setMinus` h)
          possibilities = permutations [1..size] \\ preceding

parOptimalPlay :: [Maybe Int] -> [Move] -> [Move]
parOptimalPlay clues preceding = 
    let result = GL.bigotimes (parEpsilons preceding) (pPar clues preceding)
    in preceding ++ result

-- Determine a solution that matches the given clues.
getSolution :: [Maybe Int] -> [Int]
getSolution clues = 
    let allPossibleStarts :: [[Move]]
        allPossibleStarts = map (:[]) $ permutations [1..size]
        results :: [Move]
        results = flip GL.find (p clues) $
            parMap rdeepseq (parOptimalPlay clues) allPossibleStarts
    in if null results then [] else concat results

-- Display puzzles in a human readable format.
prettyPrint :: [Move] -> String
prettyPrint ms = 
        -- The top, and the horizontal lines for each row...
        divider ++ intercalate divider  
        -- Put bars '|' at the sides, and separate each element with one.
        (map ( (\s -> "| " ++ s ++ " |") . intercalate " | " . 
        -- Turn each number into a string
        map  show )  ms ) ++ divider
    where divider = " \n " ++ unwords (replicate size "———") ++  " \n"
