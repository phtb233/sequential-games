module Handler.Sudoku where
import Import 
import Logic.Sudoku (getSolution)
import Utils (logInfoHelper)

getSudokuR :: Handler Html
getSudokuR = defaultLayout $ do
    setTitle "Sudoku"
    $(widgetFile "sudoku")

postSudokuR :: Handler Value
postSudokuR = do
    clues <- requireCheckJsonBody :: Handler [Maybe Int]
    let solution = getSolution clues
    logInfoHelper "postSudokuR" solution
    returnJson solution
