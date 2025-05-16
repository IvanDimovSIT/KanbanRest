module Main where

import Controller
import Database.PostgreSQL.Simple
import Configuration

main :: IO ()
main = do
    (jwtSecret, dbCon) <- loadConfig
    kanbanREST dbCon jwtSecret
