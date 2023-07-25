module Handler.NQueens where

import Import
import Logic.NQueens

getNQueensR :: Handler Html
getNQueensR = defaultLayout $ do
    $(widgetFile "nqueens")

-- Find the solution to the NQueens problem serialized in the request's body as
-- JSON.
postNQueensR :: Handler Value
postNQueensR = do
        input <- requireCheckJsonBody :: Handler [Maybe Int]
        let solution = getSolution input
        liftIO $ print solution
        returnJson solution
