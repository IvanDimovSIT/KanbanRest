{-# LANGUAGE DeriveGeneric #-}

module Persistence where

import Database.PostgreSQL.Simple
import Data.UUID (UUID)
import GHC.Generics ( Generic )
import Data.Aeson (ToJSON)


data UserModel = UserModel {
        userId :: UUID,
        userEmail :: String,
        userPasswordHash :: String
    } deriving (Generic, Show)

instance FromRow UserModel
instance ToJSON UserModel
    
    
data BoardModel = BoardModel {
        boardId :: UUID,
        boardName :: String,
        boardUserId :: UUID
    } deriving (Generic, Show)

instance FromRow BoardModel
instance ToJSON BoardModel
   