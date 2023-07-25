module Logic.TicTacToe where
import qualified Logic.GameLogic as GL
import Prelude
import Data.List ((\\))
import Control.Parallel.Strategies (parMap, rdeepseq)

type R = Int
type Move = Int
type Board = ([Move], [Move])

data Player = X | O deriving (Show, Eq)

p :: [Move] -> R
p ms = value (outcome X ms ([], []))

pPar :: [Move] -> [Move] -> R
pPar preceding ms = value (outcome X (preceding ++ ms) ([], []))

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = take (9 - (length preceding)) all'
    where all = epsilonX : epsilonO : all
          all' = drop (length preceding) all
          epsilonX h = GL.argsup (poolOfMoves `GL.setMinus` h)
          epsilonO h = GL.arginf (poolOfMoves `GL.setMinus` h)
          poolOfMoves = [0..8] \\ preceding

parOptimalPlay :: [Move] -> [Move]
parOptimalPlay moves = moves ++ (optimalStrategy moves)

value :: Board -> R
value (x, o) | wins x    = 1
             | wins o    = -1
             | otherwise = 0

outcome :: Player -> [Move] -> Board -> Board
outcome _ [] board = board
outcome X (m : ms) (x, o) =
    if wins o then (x, o)
    else outcome O ms (GL.insert m x, o)
outcome O (m :ms) (x, o) =
    if wins x then (x, o)
    else outcome X ms (x, GL.insert m o)
    
wins :: [Move] -> Bool
wins = 
    GL.someContained [[0,1,2],[3,4,5],[6,7,8],
                [0,3,6],[1,4,7],[2,5,8],
                [0,4,8],[2,4,6]]

optimalPlay :: [Move]
optimalPlay = GL.bigotimes GL.epsilons p

optimalOutcome :: R
optimalOutcome = p optimalPlay

--optimalStrategy :: [Move] -> Move
--optimalStrategy as   = head(GL.bigotimes epsilons' p')
    --where epsilons'  = drop (length as) GL.epsilons
          --p'      xs = p(as ++ xs)


optimalStrategy :: [Move] -> [Move]
optimalStrategy moves = GL.bigotimes (parEpsilons moves) (pPar moves)

nextMove :: [Move] -> Move
nextMove moves = 
    let possibleNextMoves = map (:[]) $ [0..8] \\ moves
        possiblePlays = map (moves ++) possibleNextMoves
        results = parMap rdeepseq parOptimalPlay possiblePlays
        optimalMoves = if even (length moves)
                           then xwins results
                           else owins results
        optimalMove  = head $ optimalMoves \\ moves
        xwins ms  =  GL.argsup ms p
        owins ms  =  GL.arginf ms p
    in  optimalMove

-- Turn based match with the computer.
-- TODO: display tictactoe grid.
playMatch :: IO ()
playMatch = 
    do
    let getMove :: [Move] -> IO Move
        getMove history = do
            putStrLn ( "history = " ++ show history)
            putStrLn "What is your next move?"
            input <- fmap reads getLine :: IO [(Int, String)]
            if null input 
                then putStrLn "Invalid input" >> getMove history
                else let move = fst $ head input
                     in if move `elem` history
                           then putStrLn "This move is taken." >> 
                                  getMove history
                           else  if move < 0 || move > 8
                                     then putStrLn "Must be within 0 and 8." >>
                                        getMove history
                                     else return move
    let gameLoop :: [Move] -> Player -> Board -> IO ()
        gameLoop history player board
            | value board == 1 = putStrLn "Player X wins"
            | value board == -1 = putStrLn "Player O wins"
            | length history == 9 = putStrLn "Its a draw"
            | otherwise = if player == X 
                            then do
                                 next <- fmap (:[]) $ getMove history
                                 let board' = outcome X next board
                                 gameLoop (history ++ next) O board'
                            else do
                                 let next = (:[]) $ nextMove history
                                 let board' = outcome O next board
                                 gameLoop (history ++ next) X board'
    gameLoop [] X ([],[])

main :: IO ()
main = undefined
