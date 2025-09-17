{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Lib
import Web.Scotty
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as TIO
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static

instance ToJSON   Message
instance FromJSON Message

main :: IO ()
main = do
    messages <- newIORef([] :: [Message])

    scotty 3000 $ do
        middleware $ staticPolicy (addBase "static")

        -- retorna a pagina html
        get "/" $ do
            page <- liftIO $ TIO.readFile "static/index.html"
            html page

        -- rota para envio de mensagens
        post "/msg" $ do
            new_message <- jsonData
            liftIO $ modifyIORef messages (new_message :)
            json new_message
        
        -- rota para recuperar todas as mensagens
        get "/msgs" $ do
            all_messages <- liftIO $ readIORef messages
            json all_messages

        -- rota para recuperar as mensagens entre dois usuarios
        get "/chat/:user1/:user2" $ do
            user1 <- param "user1"
            user2 <- param "user2"
            all_messages <- liftIO $ readIORef messages

            -- filter_messages definida em Lib.hs
            json (filter_messages user1 user2 all_messages)