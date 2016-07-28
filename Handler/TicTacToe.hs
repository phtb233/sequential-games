{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Handler.TicTacToe where

import Import
import Logic.TicTacToe (nextMove)
import Chat.Chat
import Handler.Helper

getTicTacToeR :: Handler Html
getTicTacToeR = do
        changeId
        defaultLayout $ do 
            $(widgetFile "tictactoe")
            chatWidget ChatR
        

-- Accept a request, specifying the moves that have been taken, and respond
-- with an optimal move.
postTicTacToeR :: Handler Value
postTicTacToeR = do
        moves <- requireJsonBody :: Handler [Int]
        let nmove = nextMove moves
        returnJson $ nmove 

borderStyle :: String
borderStyle = "3px solid #EEE;"

