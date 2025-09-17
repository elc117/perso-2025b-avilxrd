{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Text (Text)
import GHC.Generics

data Message = Message
    {   
        -- msg_read    :: Bool,
        -- msg_id      :: Int,    
        user_from   :: Text,
        user_to     :: Text,
        msg_content :: Text
    } deriving (Show, Generic, Eq)

-- recebe dois usuarios e uma lista de mensagens
-- retorna as mensagens entre os dois usuarios
filter_messages :: Text -> Text -> [Message] -> [Message]
filter_messages user1 user2 all_messages = filter pred all_messages
    where 
        pred msg = 
            (user_from msg == user1 && user_to msg == user2) ||
            (user_from msg == user2 && user_to msg == user1) 