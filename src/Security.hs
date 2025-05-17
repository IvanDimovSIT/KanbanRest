{-# LANGUAGE OverloadedStrings #-}
module Security(createToken, KanbanJwtClaims(..), secureOperation) where

import Web.Scotty ( ActionM, request, status, text, liftIO )
import Data.Aeson ( ToJSON(toJSON), fromJSON, Result(Success) )
import Data.Text (Text, pack, unpack, stripPrefix)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Web.JWT
import qualified Data.Map as Map
import Persistence (UserModel(..))
import Data.Map.Strict ( lookup )
import Data.UUID ( UUID, fromString )
import Network.Wai ( Request(requestHeaders) )
import Data.Time (UTCTime)
import Prelude hiding (lookup)
import qualified Data.List
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status403)


createToken :: UserModel -> String -> IO String
createToken userModel jwtSecret = do
        currentTime <- getCurrentTime
        let expTime = numericDate $ utcTimeToPOSIXSeconds $ addUTCTime validitySeconds currentTime
        return $ unpack $ encodeSigned key mempty $ createTokenWithExp expTime
    where
        validitySeconds = 8 * 3600
        createTokenWithExp expTime = mempty {
            Web.JWT.exp = expTime,
            unregisteredClaims = ClaimsMap $ Map.fromList [
                    (pack "id", toJSON (userId userModel)),
                    (pack "email", toJSON (userEmail userModel))
                ]
        }
        key = hmacSecret . pack $ jwtSecret


data KanbanJwtClaims = KanbanJwtClaims { jwtUserEmail :: String, jwtUserId :: UUID }


secureOperation :: String -> (KanbanJwtClaims -> ActionM ()) -> ActionM ()
secureOperation jwtSecret service = do
    decodedJwt <- decodeJwt jwtSecret
    case decodedJwt of
        Just kanbanJwtClaims -> service kanbanJwtClaims
        Nothing -> return ()


decodeJwt :: String -> ActionM (Maybe KanbanJwtClaims)
decodeJwt jwtSecret = do
    request' <- request
    let maybeAuthHeader = Data.List.lookup "Authorization" (requestHeaders request')
    let maybeJwt = maybeAuthHeader >>= stripPrefix "Bearer " . TE.decodeUtf8
    case maybeJwt of
        Just jwtToken -> do
            now <- liftIO getCurrentTime
            let kanbanClaims = parseJwt jwtSecret jwtToken now
            case kanbanClaims of
                Just validClaims -> return $ Just validClaims
                Nothing -> forbidden
        Nothing -> forbidden
    where
        forbidden = do
            status status403
            text "Invalid jwt token"
            return Nothing


parseJwt :: String -> Text -> UTCTime -> Maybe KanbanJwtClaims
parseJwt jwtSecret jwtToken now = do
    let unverifiedClaims = Web.JWT.decode jwtToken
    verifiedClaims <- verify (toVerify secret) =<< unverifiedClaims
    let extractedClaims = claims verifiedClaims
    claimsExp <- Web.JWT.exp extractedClaims
    currentTimeUTC <- numericDate (utcTimeToPOSIXSeconds now)
    if currentTimeUTC > claimsExp
    then Nothing
    else do
        let claimsMap = unClaimsMap $ unregisteredClaims extractedClaims
        jwtId <- lookup "id" claimsMap
        jwtEmail <- lookup "email" claimsMap
        case fromJSON jwtId of
            Success uuidStr -> case fromJSON jwtEmail of
                Success emailStr -> do
                    uuid <- fromString uuidStr
                    Just KanbanJwtClaims {
                        jwtUserId = uuid,
                        jwtUserEmail = emailStr
                    }
                _ -> Nothing
            _ -> Nothing
    where
        secret = hmacSecret . pack $ jwtSecret
