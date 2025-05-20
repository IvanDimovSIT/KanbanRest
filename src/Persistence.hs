{-# LANGUAGE DeriveGeneric #-}

module Persistence where

import Database.PostgreSQL.Simple
import Data.UUID (UUID)
import GHC.Generics ( Generic )
import Data.Aeson (ToJSON)
import Data.Time (LocalTime)


data UserModel = UserModel {
        modelUserId :: UUID,
        modelUserEmail :: String,
        modelUserPasswordHash :: String
    } deriving (Generic, Show)

instance FromRow UserModel
instance ToJSON UserModel
    
    
data BoardModel = BoardModel {
        modelBoardId :: UUID,
        modelBoardName :: String,
        modelBoardUserId :: UUID
    } deriving (Generic, Show)

instance FromRow BoardModel
instance ToJSON BoardModel
   
data TaskModel = TaskModel {
        modelTaskId :: UUID,
        modelTaskBoardId :: UUID,
        modelCreatedBy :: UUID,
        modelStatusId :: Int,
        modelAssigned :: Maybe UUID,
        modelTitle :: String,
        modelContents :: String,
        modelCreatedAt :: LocalTime,
        modelUpdatedAt :: LocalTime
    } deriving (Generic, Show)

instance FromRow TaskModel
instance ToJSON TaskModel

statusCodeName :: Int -> String
statusCodeName 1 = "To do"
statusCodeName 2 = "In progress"
statusCodeName 3 = "Merge request"
statusCodeName 4 = "In QA"
statusCodeName 5 = "Done"
statusCodeName 6 = "Cancelled"
statusCodeName _ = "UNKNOWN"
