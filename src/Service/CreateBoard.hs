{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Service.CreateBoard (createBoard) where

import Data.Aeson ( ToJSON, FromJSON )
import Web.Scotty
import Data.UUID ( UUID )
import Data.UUID.V4 ( nextRandom )
import Network.HTTP.Types ( status400, status201 )
import Database.PostgreSQL.Simple ( Connection, Only(Only), query )
import Security ( decodeJwt, KanbanJwtClaims(jwtUserId) )
import GHC.Generics ( Generic )


data CreateBoardInput = CreateBoardInput {
        boardName :: String
    } deriving (Generic, Show)
instance FromJSON CreateBoardInput


data CreateBoardOutput = CreateBoardOutput { boardId :: UUID } deriving (Generic, Show)
instance ToJSON CreateBoardOutput


createBoard :: Connection -> String -> ActionM ()
createBoard dbCon jwtSecret = do
    decodedJwt <- decodeJwt jwtSecret
    case decodedJwt of
        Just decodedJwt -> createBoardAuthenticated dbCon decodedJwt
        Nothing -> return () 


createBoardAuthenticated :: Connection -> KanbanJwtClaims -> ActionM ()
createBoardAuthenticated dbCon decodedJwt = do
    (CreateBoardInput inputName) <- jsonData
    generatedId <- liftIO $ nextRandom
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