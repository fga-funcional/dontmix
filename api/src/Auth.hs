{-# LANGUAGE OverloadedStrings #-}

module Auth
    ( getAccessToken
    ) where
        
import Control.Lens
import Data.Aeson.Lens (_String, key, nth)
import Network.Wreq as Wreq
import Data.Text
import Data.Text.Encoding
import Data.ByteString
import Data.ByteString.Base64 as Base64
import Data.ByteString.Char8 as Char8

getAccessToken :: IO (ByteString)
getAccessToken = do
    let url = "https://accounts.spotify.com/api/token"
    let raw_client_token = Base64.encode("ecb1f82367e64c61aedfc4c549c73809:b62e0ec0451c460daab6598afe3ffb74")
    let client_token =  Char8.append (Char8.pack "Basic ") (raw_client_token)
    let opts = defaults 
                & Wreq.header "Authorization" .~ [client_token] 
                & Wreq.header "Content-Type" .~ ["application/x-www-form-urlencoded"]
    let grantType = "client_credentials" :: ByteString

    r <- Wreq.postWith opts url ["grant_type" := grantType]

    let access = (r ^? responseBody . key "access_token" . _String)
    return $ encodeToken $ access

encodeToken :: Maybe Text -> ByteString
encodeToken token = 
    Char8.append (bearer) (maybe "" encodeUtf8 token)
    where
        bearer =
            Char8.pack "Bearer "