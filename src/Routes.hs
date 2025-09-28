{-# LANGUAGE OverloadedStrings #-}

module Routes (setupRoutes) where

import Web.Scotty
import Database.SQLite.Simple (Connection)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text as T
import Database
import Data.Text (Text)

setupRoutes :: Connection -> ScottyM ()
setupRoutes conn = do
    get "/" $ do
        page <- liftIO $ TIO.readFile "static/index.html"
        html page

    post "/msg" $ do
        new_message <- jsonData
        liftIO $ insert_message conn new_message
        json new_message

    get "/msgs" $ do
        all_messages <- liftIO $ get_all_messages conn
        json all_messages

    get "/chat/:user1/:user2" $ do
        user1 <- param "user1"
        user2 <- param "user2"
        all_messages <- liftIO $ get_chat conn user1 user2
        json all_messages

    get "/chat/:user1/:user2/last" $ do
        user1 <- param "user1"
        user2 <- param "user2"
        all_messages <- liftIO $ get_chat conn user1 user2
        let last_message = case all_messages of
                [] -> Nothing
                msgs -> Just (last msgs)
        json last_message
