{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.ChangeStatus(changeStatus) where
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
import Security
import Web.Scotty
import Data.UUID (UUID, fromString)
import Data.Aeson (FromJSON)
import Persistence
import Network.HTTP.Types (status404, status500, status204, status403, status400)

newtype ChangeStatusInput = ChangeStatusInput {
        taskStatus :: String
    } deriving (Generic)

instance FromJSON ChangeStatusInput

type TaskIdStatusId = (UUID, Int)

changeStatus :: Connection -> KanbanJwtClaims -> ActionM ()
changeStatus dbCon decodedJwtClaim = do
    endpointInput <- getEndpointInput
    case endpointInput of
        Just taskIdStatusId -> performOperation dbCon taskIdStatusId userId
        Nothing -> return ()
    where
        userId = jwtUserId decodedJwtClaim


performOperation :: Connection -> TaskIdStatusId -> UUID -> ActionM ()
performOperation dbCon task@(taskId, _) userId = do
    taskModel <- liftIO $ fetchTask dbCon taskId
    case taskModel of
        Just taskModel -> do
            boardModel <- liftIO $ fetchBoard dbCon (modelTaskBoardId taskModel)
            case boardModel of
                Just boardModel -> do
                    let canPerformUpdate = checkCanPerformUpdate userId taskModel boardModel
                    if canPerformUpdate
                        then performDbUpdate dbCon task
                        else do
                            status status403
                            text "You cannot update this task"
                            return ()
                Nothing -> do
                    status status404
                    text "Board not found"
                    return ()
        Nothing -> do
            status status404
            text "Task not found"
            return ()


performDbUpdate :: Connection -> TaskIdStatusId -> ActionM ()
performDbUpdate dbCon (taskId, statusId) = do
    updateResult <- liftIO $ execute dbCon
            "UPDATE tasks SET status_id = ? WHERE id = ?"
            (statusId, taskId)
    if updateResult /= 1
        then do
            status status500
            text "Error performing db operation"
        else do
            status status204
            json ()


checkCanPerformUpdate :: UUID -> TaskModel -> BoardModel -> Bool
checkCanPerformUpdate userId taskModel boardModel = modelBoardUserId boardModel == userId ||
        modelCreatedBy taskModel == userId ||
        Just userId == modelAssigned taskModel


getEndpointInput :: ActionM (Maybe TaskIdStatusId)
getEndpointInput = do
    changeStatusInput <- jsonData
    taskIdString <- captureParam "taskId"
    let taskId = fromString taskIdString
    case taskId of
        Just taskId -> do
            let maybeStatusCode = codeNameToStatus $ taskStatus changeStatusInput
            case maybeStatusCode of
                Just statusCode -> return $ Just (taskId, statusCode)
                Nothing -> do
                    status status400
                    text "Invalid task status"
                    return Nothing
        Nothing -> do
            status status400
            text "Invalid UUID format"
            return Nothing
