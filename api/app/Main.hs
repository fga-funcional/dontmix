{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Web.Scotty as S
import Data.Monoid (mconcat)
import Network.Wreq as W
import Control.Lens
import Data.Aeson (toJSON)
import Data.Aeson.Lens (_String, key, nth)
import Data.Text
import Data.Text.Encoding
import Data.Text.Lazy.Encoding as E
-- import Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as L
import Control.Monad.IO.Class
import Network.Wai.Middleware.Cors

main :: IO ()
main = scotty 3000 $ do
    middleware simpleCors    
    S.get "/search/:query" $ do
        query <- S.param "query"
        getMusic query
        



-- getMusic :: [Char] -> IO (ByteString) 
getMusic query = do
    let url =  "https://api.spotify.com/v1/search?type=track&limit=5&q=" ++ query
    let opts = defaults & W.header "Authorization" .~ ["Bearer BQC5i9vENMXdKnAj3eDrVIUHkpwCBCYSnuwfLQBmrwkCFM5_83mz9aPvEnaxEFCP5SIskdTigRJkHOiu-pxJUVtFdKHaytkE0tPCxhvatITLEpCbESzymSm0xVUF4p4Oa4aQHYUqfSfe0mvEmEo"]
    
    r <-  liftIO $ W.getWith opts url
    -- r <- getWith opts url
    raw  (r ^. responseBody) 

-- IO (Text)
-- convertByteStringToText query = do
--     converted <- fmap (E.decodeUtf8)  (getMusic query)
--     return converted
