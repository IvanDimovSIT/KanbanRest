{-# LANGUAGE DeriveGeneric #-}

module Persistence where

import Database.PostgreSQL.Simple
import Data.UUID (UUID)
import GHC.Generics
import Data.Aeson (ToJSON)

postgresConnection :: ConnectInfo
postgresConnection = defaultConnectInfo {
        connectHost = "localhost",
        connectPort = 8081,
        connectDatabase = "kanban",
        connectUser = "postgres",
        connectPassword = "postgres"
    }

data UserModel = UserModel {
        userId :: UUID,
        userEmail :: String,
        userPasswordHash :: String
    } deriving (Generic, Show)

instance FromRow UserModel
instance ToJSON UserModel
    