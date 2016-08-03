{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Chat.Chat where

import Chat.Data
import Yesod
import ClassyPrelude
import Settings (widgetFile)

instance YesodChat master => YesodSubDispatch Chat (HandlerT master IO) where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesChat)

chatWidget :: YesodChat master
           => (Route Chat -> Route master) -> WidgetT master IO ()
chatWidget toMaster = do
        chat    <- newIdent     -- the containing div
        output  <- newIdent     -- the box containing the messages
        input   <- newIdent     -- input field from the user
        send    <- newIdent     -- the send button
        uname   <- newIdent     -- input field for logging in
        login   <- newIdent     -- button for logging in
        message <- newIdent     -- invalid login message
        notice  <- newIdent     -- "You must be logged in to chat"
        {-ili <- handlerToWidget isLoggedIn -}
        {-let ili = False-}
        let messageColor = "#AAA" :: Text
            fontSize     = "0.9em"  :: Text
        $(widgetFile "chat")
