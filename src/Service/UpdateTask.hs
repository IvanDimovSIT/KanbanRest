{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.UpdateTask(updateTask) where
import Security (KanbanJwtClaims (jwtUserId))
import Validation
import Database.PostgreSQL.Simple
import Web.Scotty
import Data.UUID (UUID, fromString)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Persistence (codeNameToStatus, TaskModel (modelCreatedBy, modelTaskBoardId), fetchBoard, fetchTask, BoardModel, modelBoardUserId)
import Network.HTTP.Types (status404, status204, status500, status403, status400)

data UpdateTaskInput = UpdateTaskInput {
        taskName :: String,
        taskDescription :: String,
        taskStatus :: String,
        assigned :: Maybe UUID
    } deriving (Generic)

instance FromJSON UpdateTaskInput

validateInput :: Validator -> UpdateTaskInput -> ActionM Bool
validateInput validator updateTaskInput = validate [
            validateTaskName validator (taskName updateTaskInput),
            validateTaskDescription validator (taskDescription updateTaskInput),
            case codeNameToStatus (taskStatus updateTaskInput) of
                Just _ -> Right ()
                Nothing -> Left ("Invalid status:" ++ taskStatus updateTaskInput)
        ]

checkCanUpdate :: TaskModel -> KanbanJwtClaims -> BoardModel -> ActionM Bool
checkCanUpdate taskModel kanbanJwtClaims boardModel
    | loggedInUserId == taskCreator || loggedInUserId == boardCreator = return True
    | otherwise = do
        status status403
        text "No update access to this task"
        return False
    where    
        loggedInUserId = jwtUserId kanbanJwtClaims
        taskCreator = modelCreatedBy taskModel
        boardCreator = modelBoardUserId boardModel
        

fetchTaskAndBoard :: Connection -> UUID -> ActionM (Maybe (TaskModel, BoardModel)) 
fetchTaskAndBoard dbCon taskId = do
    taskModel <- liftIO $ fetchTask dbCon taskId
    case taskModel of
        Just taskModel ->do
            boardModel <- liftIO $ fetchBoard dbCon (modelTaskBoardId taskModel)
            case boardModel of
                Just boardModel -> return $ Just (taskModel, boardModel)
                Nothing -> do
                    status status404
                    text "board not found"
                    return Nothing
        Nothing -> do
            status status404
            text "task not found"
            return Nothing


performUpdate :: Connection -> UUID -> UpdateTaskInput -> ActionM ()
performUpdate dbCon taskId updateTaskInput = do
    updateResult <- liftIO $ execute dbCon 
            "UPDATE tasks SET title = ?, contents = ?, assigned = ?, status_id = ? WHERE id = ?"
            (taskName', taskDescription', assigned', taskStatus', taskId)
    if updateResult /= 1
        then do
            status status500
            text "Error performing db operation"
        else do
            status status204
            json ()
    where
        taskName' = taskName updateTaskInput
        taskDescription' = taskDescription updateTaskInput
        assigned' = assigned updateTaskInput
        taskStatus' = codeNameToStatus $ taskStatus updateTaskInput


getEndpointInput :: ActionM (Maybe (UpdateTaskInput, UUID))
getEndpointInput = do
    updateTaskInput <- jsonData
    taskIdString <- captureParam "taskId"
    let taskId = fromString taskIdString
    case taskId of
        Just taskId -> return $ Just (updateTaskInput, taskId)
        Nothing -> do
            status status400
            text "Invalid UUID format"
            return Nothing


updateTask :: Connection -> Validator -> KanbanJwtClaims -> ActionM ()
updateTask dbCon validator decodedJwt = do
    endpointInputs <- getEndpointInput
    case endpointInputs of
        Just (updateTaskInput, taskId) -> do
            fetchResult <- fetchTaskAndBoard dbCon taskId
            case fetchResult of
                Just (taskModel, boardModel) -> do
                    validationResult <- validateInput validator updateTaskInput
                    checkCanUpdateResult <- checkCanUpdate taskModel decodedJwt boardModel
                    if validationResult && checkCanUpdateResult
                        then performUpdate dbCon taskId updateTaskInput
                        else return ()
                Nothing -> return ()
        Nothing -> return ()
    
