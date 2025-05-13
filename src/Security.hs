module Security where

import Web.Scotty
import Network.Wai
import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (getCurrentTime, addUTCTime, nominalDay)
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Web.JWT
import qualified Data.Map as Map
import Persistence (UserModel(..))

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

authMiddleware :: Middleware
authMiddleware app req respond = do
    error "to impl"