module Logic.Connect3 where

import qualified Logic.GameLogic as GL
import Data.List (unionBy, intercalate, sortBy, sort, (\\))
import Data.Maybe (isNothing)
import Control.Parallel.Strategies
import Control.DeepSeq (force, deepseq, NFData)
import Control.Monad (guard)
import Prelude

type R = Int
type Move = Int  
type Coordinate = Int
type Position = (Coordinate, Coordinate, Player)
type Board    = [Position]

width, height, winningAmount :: Int
height =        3
width  =        3
winningAmount = 3

data Player = X | O deriving (Eq, Read, Show, Ord)

takeTurn :: Player -> Move -> Board -> Board
takeTurn p m b = let col = filter (\(x,_,_) -> x == m) b
                     len = length col
                     in  if len < height then b ++ [(m, len, p)] 
                                    else error $ "Over the top."
                                        ++ "\n move : " ++ show m 
                                        ++ "\n board : " ++ show b

-- Convert a list of moves to alternating turns, starting with player X.
movesToBoard :: [Move] -> Board
movesToBoard moves = foldl func [] pandm 
        where pandm = zip players moves 
              func b (p,m) | m > width - 1 || m < 0   = 
                                            error "Move outta bounds."
                           | wins X b || wins O b = b
                           | otherwise            = takeTurn p m b
              players :: [Player] -- Alternating turns of X and O 
              players = X : O : players

value :: Board -> R
value b 
   | wins X b  = 1
   | wins O b  = -1
   | otherwise = 0

-- Check the board vertically, horizontally and diagonally to see if the 
-- player has matched 3 in a row.
wins :: Player -> Board -> Bool
wins p b = or v || or h || or d
    -- Very verbose and clumsy check for a match of 3 in a row vertically,
    -- horizontally, and diagonally.
    where v, h :: [Bool]
          winningPlay   = replicate winningAmount p
          -- Get all this player's moves.
          actualPlays = filter (\(_,_,p') -> p' == p) b
          v = do  
              -- Check each column for a vertical match of 3 in a row. 
              x <- [0..width - 1]
              let section = map (\(_,_,p') -> p') $ 
                    filter (\(x',_,_) -> x' == x) b
              return $ GL.contained winningPlay section
          h = do
              -- Check each row for a horizontal match of 3 in a row.
              y <- [0..height - 1]
              let section = filter (\(_,y',_) -> y' == y) actualPlays
              return $ length section == winningAmount
              -- Combine the result of checking the left and right
              -- for diagonal matches of 3 in a row.
          d :: [Bool]
          -- ld = descending towards the left corner (/), rd = descending to
          -- the right (\).
          d = do
                -- Check every possible diagonal, right to left, for
                diff <- [winningAmount - width..height - winningAmount]
                let ld = map (\(_,_,p') -> p') $
                        filter (\(x',y',_) -> y' == x' + diff) actualPlays
                let rd = map (\(_,_,p') -> p') $
                        filter (\(x',y',_) -> y' == pred winningAmount - x' 
                        + diff) actualPlays
                return $ (GL.contained winningPlay rd) ||
                          GL.contained winningPlay ld

p :: [Move] -> R
p ms = value (movesToBoard ms)

pPar :: [Move] -> [Move] -> R
pPar preceding ms = value (movesToBoard $ preceding ++ ms)

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = take ((width * height) - length preceding) all'
    where all = epsilonX : epsilonO : all
          all' = drop (length preceding) all
          epsilonX h = GL.argsup (poolOfMoves `GL.setMinus` h)
          epsilonO h = GL.arginf (poolOfMoves `GL.setMinus` h)
          poolOfMoves = sort ((concat $ replicate height [0..width - 1])
                          \\ preceding)

parOptimalPlay :: [Move] -> [Move]
parOptimalPlay moves = let result = GL.bigotimes (parEpsilons moves) 
                                        (pPar moves)
                       in moves ++ result

epsilons :: [[Move] -> GL.J R Move]
epsilons = take (height * width) all
       where all = epsilonX : epsilonO : all
             epsilonX h = GL.argsup (poolOfMoves `GL.setMinus` h)
             epsilonO h = GL.arginf (poolOfMoves `GL.setMinus` h)
             -- For some reason, it wont work unless sorted.
             poolOfMoves = sort $ concat $ replicate height [0..width - 1]

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

optimalOutcome :: R
optimalOutcome = p optimalPlay

-- Human readable Board.
prettyPrint :: Board -> String
prettyPrint b =
        -- Populate a list of filled/empty spaces for the connect 4 grid.
    let rows :: [[(Coordinate, Coordinate, Maybe Player)]]
        rows = do 
                y <- [1..height]
                -- A list of empty spaces we'll combine with the filled ones
                let emptySpaces :: [(Coordinate, Coordinate, Maybe a)]
                    emptySpaces = zip3 [1..width] (repeat y) (repeat Nothing)
                return $ sortBy order $ flip (unionBy isBlank) emptySpaces $ 
                    map (\(a,b,c) -> (a,b,Just c)) $
                    filter (\(_,y',_) -> y' == y) b
        -- If second tuple contains Nothing, return True.
        isBlank :: (Coordinate, Coordinate, Maybe Player) ->
                    (Coordinate, Coordinate, Maybe Player) -> Bool
        isBlank (x,y,m) (x',y',m') = x == x' && y == y' && isNothing m'
        -- Organize rows by their x coordinate.
        order :: (Coordinate, Coordinate, Maybe Player) ->
                  (Coordinate, Coordinate, Maybe Player) -> Ordering
        order (x,_,_) (x',_,_) = compare x x'
        line :: String
        line = "\n" ++ concat ( replicate width " ―――") ++ "\n"
    in 
        intercalate line $ map ( (\s -> "  " ++ s) . intercalate " | " . 
            map (\(_,_,p) -> maybe " " show p)) (reverse rows)

nextMove :: [Move] -> Move
nextMove moves = 
    let possiblePlays = do
            x <- [0..width - 1]
            -- We only consider possibilities where that column isn't full.
            guard ((< height) $ length $ filter (== x) moves)
            return $ moves ++ [x]
        results = parMap rdeepseq parOptimalPlay possiblePlays
        -- Get the winning move for this particuluar player (presuming
        -- X goes first).
        optimalMoves = if even (length moves)
                           then xwins results
                           else owins results
        optimalMove  = head $ optimalMoves \\ moves
        xwins ms  =  GL.argsup ms p
        owins ms  =  GL.arginf ms p
    in optimalMove

main :: IO ()
main = do
         let allPossibleStarts = (map (:[]) [0..width - 1])
             results = parMap rdeepseq parOptimalPlay allPossibleStarts
             optimalMoves = GL.argsup results p
             message 
                | p optimalMoves == 1 = "win"
                | p optimalMoves == 0 = "draw"
                | otherwise           = "lose"
         putStrLn $ "For a game of Connect" ++ show winningAmount ++
                  " on a " ++ show width ++ "x" ++ show height ++ " grid;"
         putStrLn $ "X " ++ message ++ "s : " ++ show optimalMoves
