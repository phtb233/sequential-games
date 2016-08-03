module Handler.Sudoku where
import Import 
import Logic.Sudoku (getSolution)

getSudokuR :: Handler Html
getSudokuR = defaultLayout $ do
    setTitle "Sudoku"
    $(widgetFile "sudoku")

postSudokuR :: Handler Value
postSudokuR = do
        clues <- requireJsonBody :: Handler [Maybe Int]
        {-liftIO $ print $ length clues-}
        let solution = getSolution clues
        liftIO $ print solution
        returnJson solution
