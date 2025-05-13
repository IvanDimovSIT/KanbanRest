{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.LoginUser where

import Web.Scotty
import Data.Text
import Data.Aeson
import GHC.Generics
import Database.PostgreSQL.Simple
import Data.Password.Argon2
import Network.HTTP.Types
import Persistence
import Security

data LoginUserInput = LoginUserInput {
        email :: String,
        password :: String
    }deriving (Generic, Show)
instance FromJSON LoginUserInput

data LoginUserOutput = LoginUserOutput { jwtToken :: String } deriving (Generic, Show)
instance ToJSON LoginUserOutput

loginUser :: Connection -> String -> ActionM ()
loginUser dbCon jwtSecret = do
    (LoginUserInput email password) <- jsonData
    let selectQuery = query dbCon
            "SELECT * FROM users WHERE email = ?"
            (Only email)
    result <- liftIO selectQuery
    case result of
        (userModel : []) -> do
                let isValidPassword = validatePassword password (userPasswordHash userModel)
                if isValidPassword
                then do
                    userJwtToken <- liftIO $ createToken userModel jwtSecret
                    json LoginUserOutput { jwtToken = userJwtToken }
                else invalidEmailOrPassword
        _ -> invalidEmailOrPassword
    where
        invalidEmailOrPassword = do
            status status403
            text "Incorrect email or password"


validatePassword :: String -> String -> Bool
validatePassword inputPass correctPasswordHash = isValid $ checkPassword (mkPassword $ pack inputPass) PasswordHash{ unPasswordHash = pack correctPasswordHash}
    where
        isValid PasswordCheckSuccess = True
        isValid PasswordCheckFail = False
