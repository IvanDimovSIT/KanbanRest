module Main where

import Controller
import Persistence
import Database.PostgreSQL.Simple

main :: IO ()
main = do
    con <- connect postgresConnection
    kanbanREST con "12345678" -- TODO: Load secret
