module Validation where

import Web.Scotty
import Network.HTTP.Types ( status400 )
import Data.List ( intercalate )
import Data.Text.Lazy ( pack )
import Text.Regex.TDFA
import Data.Either (lefts)

type ValidationResult = Either String ()

validate :: [ValidationResult] -> ActionM Bool
validate validationResults = if null errors 
    then return True
    else do
        formatErrors
        return False
    where
        errors = lefts validationResults
        errorsString = intercalate "\n" errors
        formatErrors = do
            status status400
            text $ pack errorsString


data Validator = Validator { emailRegex :: Regex, passwordRegex :: Regex, boardNameRegex :: Regex }

defaultValidator :: Validator
defaultValidator = Validator {
        emailRegex = makeRegex "^[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+$",
        passwordRegex = makeRegex "^[a-zA-Z0-9+._\\*$#%@^-]+$",
        boardNameRegex = makeRegex "^[a-zA-Z0-9#$%+._\\-][a-zA-Z0-9 #$%+._\\-]+$"
    }

validateEmail :: Validator -> String -> ValidationResult
validateEmail validator email = if length email < 64 && matchTest (emailRegex validator) email
    then Right ()
    else Left "invalid email"

validatePassword :: Validator -> String -> ValidationResult
validatePassword validator password = if length password >= 8 && matchTest (passwordRegex validator) password
    then Right ()
    else Left "invalid password, needs at least 8 symbols"

validateBoardName :: Validator -> String -> ValidationResult
validateBoardName validator boardName = if len >= 3 && len < 64 && matchTest (boardNameRegex validator) boardName
    then Right ()
    else Left "invalid board name, needs to be not blank and at least 3 symbols"
    where
        len = length boardName
