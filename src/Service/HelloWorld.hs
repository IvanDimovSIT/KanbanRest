{-# LANGUAGE DeriveGeneric #-}

module Service.HelloWorld where

import Web.Scotty
import Data.Aeson
import GHC.Generics

data HelloWorldOutput = HelloWorldOutput { 
        greeting :: String 
    } deriving (Generic, Show)

instance ToJSON HelloWorldOutput

helloWorldService :: ActionM ()
helloWorldService = json HelloWorldOutput { greeting = "Hello, World!"}

