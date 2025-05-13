{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.RegisterUser where

import Web.Scotty
import Data.Text
import Data.Aeson
import GHC.Generics
import Database.PostgreSQL.Simple
import Data.UUID
import qualified Data.UUID.V4 as UUID
import Data.Password.Argon2
import Network.HTTP.Types

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
    then json RegisterUserOutput {userId = randomId}
    else do
            status status400
            text "Email taken"
