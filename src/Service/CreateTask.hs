{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Service.CreateTask (createTask) where

import Database.PostgreSQL.Simple
import Validation
import Security
import Web.Scotty
import GHC.Generics (Generic)
import Data.UUID (UUID)
import Data.Aeson (FromJSON)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Types (status404, status403, status201)
import Data.Aeson.Types (ToJSON)


data CreateTaskInput = CreateTaskInput {
        taskName :: String,
        taskDescription :: String,
        boardId :: UUID
    } deriving (Generic)

instance FromJSON CreateTaskInput

data CreateTaskOutput = CreateTaskOutput {
        taskId :: UUID
    } deriving (Generic)

instance ToJSON CreateTaskOutput

validateInput :: Validator -> CreateTaskInput -> ActionM Bool
validateInput validator (CreateTaskInput taskName taskDescription _) = validate [
            validateTaskName validator taskName,
            validateTaskDescription validator taskDescription
        ]

createTask :: Connection -> Validator -> KanbanJwtClaims -> ActionM ()
createTask dbCon validator decodedJwt = do
    (CreateTaskInput taskName taskDescription boardId) <- jsonData
    validationResult <- validateInput validator (CreateTaskInput taskName taskDescription boardId)
    if validationResult
        then createTaskOperation (CreateTaskInput taskName taskDescription boardId)
        else return ()
    where
        createTaskOperation (CreateTaskInput taskName taskDescription boardId) = do
            randomId <- liftIO nextRandom
            let creatorId = jwtUserId decodedJwt
            let insertQuery = query dbCon
                    "SELECT create_task(?, ?, ?, ?, ?)"
                    (randomId, boardId, creatorId, taskName, taskDescription)
            [Only result] <- liftIO insertQuery
            handleResult result randomId
        

handleResult :: String -> UUID -> ActionM ()
handleResult result addedId
    | result == "created" = do
        status status201
        json CreateTaskOutput { taskId = addedId } 
    | result == "forbidden" = do
        status status403
        text "You don't own this board"
    | otherwise = do
        status status404
        text "Board not found!"
           
