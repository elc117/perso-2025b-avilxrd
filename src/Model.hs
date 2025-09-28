{-# LANGUAGE DeriveGeneric #-}

module Model (Message(..)) where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson

data Message = Message
    { user_from   :: Text
    , user_to     :: Text
    , msg_content :: Text
    } deriving (Show, Generic, Eq)

instance ToJSON Message
instance FromJSON Message
