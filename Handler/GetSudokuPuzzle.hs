module Handler.GetSudokuPuzzle where

import Import

-- Respond to the client's request with a new Sudoku grid read from a local
-- file.
postGetSudokuPuzzleR :: Handler Value
postGetSudokuPuzzleR = do
    puzzles <- getPuzzles <$> getYesod
    returnJson $ puzzles
