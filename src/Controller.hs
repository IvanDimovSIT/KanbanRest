{-# LANGUAGE OverloadedStrings #-}
module Controller where

import Web.Scotty
import Service.HelloWorld
import Service.RegisterUser(registerUser)
import Service.LoginUser(loginUser)
import Database.PostgreSQL.Simple

--    get "/api/hello-world" $ do
--        middleware authMiddleware  -- Apply auth middleware only to this route
--        helloWorldService         -- Call the actual service after the middleware

kanbanREST :: Connection -> String -> IO ()
kanbanREST dbCon jwtSecret = scotty 8080 $ do
    get "/api/hello-world" helloWorldService
    post "/api/auth/register" $ registerUser dbCon
    post "/api/auth/login" $ loginUser dbCon jwtSecret