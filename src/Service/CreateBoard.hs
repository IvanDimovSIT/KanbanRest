{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Service.CreateBoard (createBoard) where

import Data.Aeson ( ToJSON, FromJSON )
import Web.Scotty
import Data.UUID ( UUID )
import Data.UUID.V4 ( nextRandom )
import Network.HTTP.Types ( status400, status201 )
import Database.PostgreSQL.Simple ( Connection, Only(Only), query )
import Security ( KanbanJwtClaims(jwtUserId) )
import GHC.Generics ( Generic )
import Validation


newtype CreateBoardInput = CreateBoardInput {
        boardName :: String
    } deriving (Generic, Show)
instance FromJSON CreateBoardInput

validateInput :: Validator -> CreateBoardInput -> ActionM Bool
validateInput validator (CreateBoardInput boardName) = validate [
            validateBoardName validator boardName
        ]


data CreateBoardOutput = CreateBoardOutput { boardId :: UUID } deriving (Generic, Show)
instance ToJSON CreateBoardOutput


createBoard :: Connection -> Validator -> KanbanJwtClaims -> ActionM ()
createBoard dbCon validator decodedJwt = do
    (CreateBoardInput inputName) <- jsonData
    isValid <- validateInput validator (CreateBoardInput inputName) 
    if isValid
        then performOperation dbCon decodedJwt (CreateBoardInput inputName)
        else return ()
    
    
performOperation :: Connection -> KanbanJwtClaims -> CreateBoardInput -> ActionM ()
performOperation dbCon decodedJwt (CreateBoardInput inputName) = do
    generatedId <- liftIO nextRandom
    let insertQuery = query dbCon
            "SELECT add_board(?, ?, ?)"
            (generatedId, inputName, jwtUserId decodedJwt)
    [Only result] <- liftIO insertQuery
    if result
    then do
        status status201
        json CreateBoardOutput {boardId = generatedId}
    else do
         status status400
         text "Name is already in use"
        