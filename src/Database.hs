{-# LANGUAGE DeriveGeneric #-}

module Database (Message(..), init_db, insert_message, get_all_messages, get_chat) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Data.String (fromString)

data Message = Message
    { user_from   :: Text
    , user_to     :: Text
    , msg_content :: Text
    } deriving (Show, Generic, Eq)

instance FromRow Message where
    fromRow = Message <$> field <*> field <*> field

instance ToJSON Message
instance FromJSON Message

init_db :: Connection -> IO ()
init_db conn = execute_ conn $ fromString
  "CREATE TABLE IF NOT EXISTS messages (\
  \ id INTEGER PRIMARY KEY AUTOINCREMENT,\
  \ user_from TEXT,\
  \ user_to TEXT,\ 
  \ msg_content TEXT)"

insert_message :: Connection -> Message -> IO ()
insert_message conn msg = execute conn
    (fromString "INSERT INTO messages (user_from, user_to, msg_content) VALUES (?,?,?)")
    (user_from msg, user_to msg, msg_content msg)

get_all_messages :: Connection -> IO [Message]
get_all_messages conn = query_ conn (fromString "SELECT user_from, user_to, msg_content FROM messages") :: IO [Message]

get_chat :: Connection -> Text -> Text -> IO [Message]
get_chat conn user1 user2 = do
    query conn
        (fromString "SELECT user_from, user_to, msg_content FROM messages WHERE \
        \ (user_from = ? AND user_to = ?) OR \
        \ (user_from = ? AND user_to = ?)")
        (user1, user2, user2, user1) :: IO [Message]
