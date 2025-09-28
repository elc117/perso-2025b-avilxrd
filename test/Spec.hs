{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Database.SQLite.Simple
import Database
import Data.Text (Text)
import Control.Exception (bracket)

-- Função auxiliar para criar um banco de dados em memória para testes
withTestDB :: (Connection -> IO a) -> IO a
withTestDB action = do
    bracket (open ":memory:") close $ \conn -> do
        init_db conn
        action conn

-- Testes
main :: IO ()
main = do
    runTestTT tests
    return ()

tests :: Test
tests = TestList
    [ TestLabel "Test Insert and Get All Messages" testInsertAndGetAllMessages
    , TestLabel "Test Get Chat" testGetChat
    , TestLabel "Test Get Last Message" testGetLastMessage
    ]

testInsertAndGetAllMessages :: Test
testInsertAndGetAllMessages = TestCase $ withTestDB $ \conn -> do
    let msg1 = Message "Alice" "Bob" "Hello Bob!"
    let msg2 = Message "Bob" "Alice" "Hi Alice!"
    
    insert_message conn msg1
    insert_message conn msg2
    
    messages <- get_all_messages conn
    assertEqual "Should return 2 messages" 2 (length messages)
    assertEqual "First message should be from Alice" (user_from msg1) (user_from (head messages))
    assertEqual "Second message should be from Bob" (user_from msg2) (user_from (messages !! 1))

testGetChat :: Test
testGetChat = TestCase $ withTestDB $ \conn -> do
    let msg1 = Message "Alice" "Bob" "Hello Bob!"
    let msg2 = Message "Bob" "Alice" "Hi Alice!"
    
    insert_message conn msg1
    insert_message conn msg2
    
    chatMessages <- get_chat conn "Alice" "Bob"
    assertEqual "Should return 2 messages in chat" 2 (length chatMessages)

testGetLastMessage :: Test
testGetLastMessage = TestCase $ withTestDB $ \conn -> do
    let msg1 = Message "Alice" "Bob" "Hello Bob!"
    let msg2 = Message "Bob" "Alice" "Hi Alice!"
    
    insert_message conn msg1
    insert_message conn msg2
    
    lastMsg <- get_chat conn "Alice" "Bob"
    let lastMessage = case lastMsg of
            [] -> Nothing
            msgs -> Just (last msgs)
    
    assertEqual "Last message should be from Bob" (Just msg2) lastMessage
