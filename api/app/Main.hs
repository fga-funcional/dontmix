{-# LANGUAGE OverloadedStrings #-}
module Main where

import Auth (getAccessToken)
import Web.Scotty as Scotty
import Network.Wreq as Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key, nth)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors

main :: IO ()
main = scotty 3000 $ do
    middleware simpleCors    
    Scotty.get "/search/:query" $ do
        query <- Scotty.param "query"
        searchMusics query
    
    Scotty.get "/recommendation/:query" $ do
        query <- Scotty.param "query"
        getRecommendation query

        
searchMusics :: [Char] -> ActionM()
searchMusics query = do
    let url =  "https://api.spotify.com/v1/search?type=track&limit=5&q=" ++ query

    access_token <- liftIO $ getAccessToken
    let opts = defaults & Wreq.header "Authorization" .~ [access_token]

    r <-  liftIO $ Wreq.getWith opts url
    raw  (r ^. responseBody) 

getRecommendation :: [Char] -> ActionM()
getRecommendation query = do
    let url =  "https://api.spotify.com/v1/recommendations?seed_tracks=" ++ query

    access_token <- liftIO $ getAccessToken
    let opts = defaults & Wreq.header "Authorization" .~ [access_token]

    r <-  liftIO $ Wreq.getWith opts url
    raw  (r ^. responseBody) 