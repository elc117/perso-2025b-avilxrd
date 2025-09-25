{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Data.IORef
import Web.Scotty
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

instance FromRow Message where
    fromRow = Message <$> field <*> field <*> field


-- a função foi descontinuada pois agora é feito um filtro no SQL
-- recebe dois usuarios e uma lista de mensagens
-- retorna as mensagens entre os dois usuarios
-- filter_messages :: Text -> Text -> [Message] -> [Message]

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

-- recupera todas as mensagens
get_all_messages :: Connection -> IO [Message]
get_all_messages conn = query_ conn "SELECT user_from, user_to, msg_content FROM messages" :: IO [Message]

-- recupera as mensagens entre dois usuarios
get_chat :: Connection -> Text -> Text -> IO [Message]
get_chat conn user1 user2 = do
    query conn
        "SELECT user_from, user_to, msg_content FROM messages WHERE \
        \ (user_from = ? AND user_to = ?) OR \
        \ (user_from = ? AND user_to = ?)"
        (user1, user2, user2, user1) :: IO [Message]


main :: IO ()
main = do
    
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
            liftIO $ insert_message conn new_message
            json new_message
        
        -- rota para recuperar todas as mensagens
        get "/msgs" $ do
            all_messages <- liftIO $ get_all_messages conn
            json all_messages

        -- rota para recuperar as mensagens entre dois usuarios
        get "/chat/:user1/:user2" $ do
            user1 <- param "user1"
            user2 <- param "user2"
            all_messages <- liftIO $ get_chat conn user1 user2

            json all_messages