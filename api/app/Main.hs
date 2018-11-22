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
        searchMusics query
    
    S.get "/recommendation/:query" $ do
        query <- S.param "query"
        getRecommendation query
        


-- getMusic :: [Char] -> IO (ByteString) 
searchMusics query = do
    let url =  "https://api.spotify.com/v1/search?type=track&limit=5&q=" ++ query
    let opts = defaults & W.header "Authorization" .~ ["Bearer BQAJFHM3GQVJP4dB8QA8qZQ9rXmKwT8Wb8AE92Fq_G6Sh3I_UtOid3vn2MOKhgIMioA5VUOYqppxDHCzTLT-eb5d2U1V1ElmrZKfeA1tYulDraxwtEyW2GvzfBPjDZ4T1QM2KV4lOJBlsdwu3Ao"]
    
    r <-  liftIO $ W.getWith opts url
    raw  (r ^. responseBody) 

getRecommendation query = do
    let url =  "https://api.spotify.com/v1/recommendations?seed_tracks=" ++ query
    let opts = defaults & W.header "Authorization" .~ ["Bearer BQAJFHM3GQVJP4dB8QA8qZQ9rXmKwT8Wb8AE92Fq_G6Sh3I_UtOid3vn2MOKhgIMioA5VUOYqppxDHCzTLT-eb5d2U1V1ElmrZKfeA1tYulDraxwtEyW2GvzfBPjDZ4T1QM2KV4lOJBlsdwu3Ao"]

    r <-  liftIO $ W.getWith opts url
    raw  (r ^. responseBody) 