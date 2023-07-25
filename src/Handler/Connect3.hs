module Handler.Connect3 where

import Import
import Logic.Connect3 (nextMove)
import Chat.Chat
import Handler.Helper
import Utils (logInfoHelper)

getConnect3R :: Handler Html
getConnect3R = defaultLayout $ do
        setTitle "Connect 3"
        $(widgetFile "connect3")
        chatWidget ChatR

-- Respond with an optimal move.
postConnect3R :: Handler Value
postConnect3R = do 
                moves <- requireCheckJsonBody
                let nmove = nextMove moves
                logInfoHelper "postConnect3R" nmove
                returnJson $ nmove
