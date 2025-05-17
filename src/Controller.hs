{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Web.Scotty
import Service.CreateBoard
import Service.RegisterUser(registerUser)
import Service.LoginUser(loginUser)
import Database.PostgreSQL.Simple
import Security
import Validation (Validator)

kanbanREST :: Connection -> String -> Validator -> IO ()
kanbanREST dbCon jwtSecret validator = scotty 8080 $ do
    post "/api/auth/register" $ registerUser dbCon validator
    post "/api/auth/login" $ loginUser dbCon jwtSecret
    post "/api/board" $ secureOperation jwtSecret (createBoard dbCon validator)