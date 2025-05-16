{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Web.Scotty
import Service.CreateBoard
import Service.RegisterUser(registerUser)
import Service.LoginUser(loginUser)
import Database.PostgreSQL.Simple


kanbanREST :: Connection -> String -> IO ()
kanbanREST dbCon jwtSecret = scotty 8080 $ do
    post "/api/auth/register" $ registerUser dbCon
    post "/api/auth/login" $ loginUser dbCon jwtSecret
    post "/api/board" $ createBoard dbCon jwtSecret