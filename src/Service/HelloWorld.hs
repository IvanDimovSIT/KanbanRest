{-# LANGUAGE DeriveGeneric #-}

module Service.HelloWorld where

import Web.Scotty
import Data.Aeson
import GHC.Generics
import Security (decodeJwt)

data HelloWorldOutput = HelloWorldOutput { 
        greeting :: String 
    } deriving (Generic, Show)

instance ToJSON HelloWorldOutput

helloWorldService :: String -> ActionM ()
helloWorldService jwtSecret = do
    decoded <- decodeJwt jwtSecret
    case decoded of
        Nothing -> return ()
        _ -> json HelloWorldOutput { greeting = "Hello, World!"}

