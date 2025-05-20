{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.LoginUser(loginUser) where

import Web.Scotty ( ActionM, liftIO, json, jsonData, status, text )
import Data.Text ( pack )
import Data.Aeson ( ToJSON, FromJSON )
import GHC.Generics ( Generic )
import Database.PostgreSQL.Simple ( Connection, Only(Only), query )
import Data.Password.Argon2
    ( checkPassword,
      mkPassword,
      PasswordCheck(PasswordCheckFail, PasswordCheckSuccess),
      PasswordHash(PasswordHash, unPasswordHash) )
import Network.HTTP.Types ( status403 )
import Security ( createToken )
import Persistence


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
                let isValidPassword = validatePassword password (modelUserPasswordHash userModel)
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
