module Handler.Sudoku where
import Import 

getSudokuR :: Handler Html
getSudokuR = defaultLayout [whamlet|<p>sudoku|]

{-postSudokuR = undefined-}
