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

data Message = Message
    {   
        msg_id      :: Int,    
        user_from   :: Text,
        user_to     :: Text,
        msg_content :: Text,
        msg_read    :: Bool
    } deriving (Show, Generic, Eq)

instance ToJSON   Message
instance FromJSON Message

main :: IO ()
main = do
    messages <- newIORef([] :: [Message])

    scotty 3000 $ do
        middleware $ staticPolicy (addBase "static")

        get "/" $ do
            page <- liftIO $ TIO.readFile "static/index.html"
            html page