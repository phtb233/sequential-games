module Handler.Connect3 where

import Import
import Logic.Connect3 (nextMove)
import Chat.Chat
import Handler.Helper

getConnect3R :: Handler Html
getConnect3R = do
        changeId
        defaultLayout $ do
            $(widgetFile "connect3")
            chatWidget ChatR

postConnect3R :: Handler Value
postConnect3R = do 
                moves <- requireJsonBody
                let nmove = nextMove moves
                liftIO $ print nmove
                returnJson $ nmove

borderStyle :: String
borderStyle = "3px solid #EEE;"
