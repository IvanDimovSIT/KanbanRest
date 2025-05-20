{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Service.GetTasks(getTasks) where
import Persistence
import Web.Scotty
import Data.UUID (UUID, fromString)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Network.HTTP.Types (status400, status200)
import Data.Text.Internal.Lazy
import Database.PostgreSQL.Simple
import Data.Time
import Text.Read (readMaybe)
import Data.Either.Extra


data TaskOutput = TaskOutput {
        taskId :: UUID,
        taskBoardId :: UUID,
        createdBy :: UUID,
        currentStatus :: String,
        assigned :: Maybe UUID,
        title :: String,
        contents :: String,
        createdAt :: LocalTime,
        updatedAt :: LocalTime
    } deriving(Generic)

instance ToJSON TaskOutput

data GetTasksOutput = GetTasksOutput {
        tasks::[TaskOutput]
    } deriving(Generic)

instance ToJSON GetTasksOutput

validate :: String -> String -> Either Text (UUID, Int)
validate boardId daysSinceLastUpdate = do
        parsedBoardId <- parseBoardId boardId
        parsedDaysSinceLastUpdate <- parseDaysSinceLastUpdate daysSinceLastUpdate
        if parsedDaysSinceLastUpdate > 0
            then Right (parsedBoardId, parsedDaysSinceLastUpdate)
            else Left "daysSinceUpdate must be positive"
    where
        parseBoardId boardId = maybeToEither "Invalid UUID format" (fromString boardId)
        parseDaysSinceLastUpdate daysSinceLastUpdate = maybeToEither "Invalid int format" (readMaybe daysSinceLastUpdate)


convertToOutput :: [TaskModel] -> GetTasksOutput
convertToOutput models = GetTasksOutput { tasks = map toOutput models }
    where
        toOutput :: TaskModel -> TaskOutput
        toOutput taskModel = TaskOutput {
                taskId = modelTaskId taskModel,
                taskBoardId = modelTaskBoardId taskModel,
                createdBy = modelCreatedBy taskModel,
                currentStatus = statusCodeName $ modelStatusId taskModel,
                assigned = modelAssigned taskModel,
                title = modelTitle taskModel,
                contents = modelContents taskModel,
                createdAt = modelCreatedAt taskModel,
                updatedAt = modelUpdatedAt taskModel
            }

getTasks :: Connection -> ActionM ()
getTasks dbCon = do
    boardId <- captureParam "boardId"
    daysSinceLastUpdate <- queryParam "daysSinceUpdate"
    case validate boardId daysSinceLastUpdate of
        Right (boardId, daysSinceLastUpdate) -> do
            let selectQuery = query dbCon
                    "SELECT * FROM tasks WHERE board_id = ? AND date_part('day', now() - updated_at) <= ?"
                    (boardId, daysSinceLastUpdate)
            tasks <- liftIO selectQuery
            status status200
            json $ convertToOutput tasks
        Left err -> do
            status status400
            text err