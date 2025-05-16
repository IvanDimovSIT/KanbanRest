{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.RegisterUser(registerUser) where

import Web.Scotty
import Data.Text ( pack )
import Data.Aeson ( ToJSON, FromJSON )
import GHC.Generics ( Generic )
import Database.PostgreSQL.Simple ( Connection, Only(Only), query )
import Data.UUID ( UUID )
import qualified Data.UUID.V4 as UUID
import Data.Password.Argon2
    ( hashPassword, mkPassword, PasswordHash(unPasswordHash) )
import Network.HTTP.Types ( status400, status201 )


data RegisterUserInput = RegisterUserInput {
        email :: String,
        password :: String
    }deriving (Generic, Show)
instance FromJSON RegisterUserInput


data RegisterUserOutput = RegisterUserOutput { userId :: UUID } deriving (Generic, Show)
instance ToJSON RegisterUserOutput


registerUser :: Connection -> ActionM ()
registerUser dbCon = do
    (RegisterUserInput email password) <- jsonData
    randomId <- liftIO UUID.nextRandom
    hashedPassword <- liftIO $ hashPassword $ mkPassword $ pack password
    let insertQuery = query dbCon
            "SELECT register_user(?, ?, ?)"
            (randomId, email, unPasswordHash hashedPassword)
    [Only result] <- liftIO insertQuery
    if result
    then do
        status status201
        json RegisterUserOutput {userId = randomId}
    else do
            status status400
            text "Email taken"
