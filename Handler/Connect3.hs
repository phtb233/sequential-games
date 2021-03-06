module Handler.Connect3 where

import Import
import Logic.Connect3 (nextMove)
import Chat.Chat
import Handler.Helper

getConnect3R :: Handler Html
getConnect3R = defaultLayout $ do
        setTitle "Connect 3"
        $(widgetFile "connect3")
        chatWidget ChatR

-- Respond with an optimal move.
postConnect3R :: Handler Value
postConnect3R = do 
                moves <- requireJsonBody
                let nmove = nextMove moves
                liftIO $ print nmove
                returnJson $ nmove
