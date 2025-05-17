module Main where

import Controller
import Configuration

main :: IO ()
main = do
    (jwtSecret, dbCon, validator) <- loadConfig
    kanbanREST dbCon jwtSecret validator
