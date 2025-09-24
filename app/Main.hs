{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Main (main) where

import Web.Scotty
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as TIO
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Message = Message
    {    
        user_from   :: Text,
        user_to     :: Text,
        msg_content :: Text
    } deriving (Show, Generic, Eq)

instance ToJSON   Message
instance FromJSON Message

-- recebe dois usuarios e uma lista de mensagens
-- retorna as mensagens entre os dois usuarios
filter_messages :: Text -> Text -> [Message] -> [Message]
filter_messages user1 user2 all_messages = filter pred all_messages
    where 
        pred msg = 
            (user_from msg == user1 && user_to msg == user2) ||
            (user_from msg == user2 && user_to msg == user1) 

-- inicializa o banco de dados
init_db :: Connection -> IO ()
init_db conn = execute_ conn
  "CREATE TABLE IF NOT EXISTS messages (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ user_from TEXT,\
  \ user_to TEXT,\ 
  \ msg_content TEXT)"

-- insere uma nova mensagem no banco de dado
insert_message :: Connection -> Message -> IO ()
insert_message conn msg = execute conn
    "INSERT INTO messages (user_from, user_to, msg_content) VALUES (?,?,?)"
    (user_from msg, user_to msg, msg_content msg)


main :: IO ()
main = do
    messages <- newIORef([] :: [Message])
    
    conn <- open "messages.db"
    init_db conn

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