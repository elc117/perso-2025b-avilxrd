{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty
import Database.SQLite.Simple
import Database
import Routes (setupRoutes)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import qualified Data.Text.Lazy.IO as TIO

main :: IO ()
main = do
    conn <- open "messages.db"
    init_db conn
    scotty 3000 $ do
        middleware $ staticPolicy (addBase "static")  -- Adicionando o middleware aqui
        setupRoutes conn
