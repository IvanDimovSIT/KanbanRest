{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Web.Scotty
import Service.HelloWorld
import Service.RegisterUser(registerUser)
import Service.LoginUser(loginUser)
import Database.PostgreSQL.Simple


kanbanREST :: Connection -> String -> IO ()
kanbanREST dbCon jwtSecret = scotty 8080 $ do
    get "/api/hello-world" $ helloWorldService jwtSecret
    post "/api/auth/register" $ registerUser dbCon
    post "/api/auth/login" $ loginUser dbCon jwtSecret