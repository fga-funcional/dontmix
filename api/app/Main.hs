{-# LANGUAGE OverloadedStrings #-}
module Main where

import Auth (getAccessToken)
import Database (findOrCreatePageJSON, editPageJSON)
import Web.Scotty as Scotty
import Network.Wreq as Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key, nth)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import Data.ByteString.Lazy.Char8 as Char8

main :: IO ()
main = scotty 3000 $ do
    middleware simpleCors
    
    Scotty.get "/:page"  findOrCreatePage

    Scotty.put "/:page" editPage

    Scotty.get "/search/:query" searchMusics
    
    Scotty.get "/recommendation/:query" getRecommendation 

findOrCreatePage :: ActionM()
findOrCreatePage = do
    pageName <- Scotty.param "page"
    filePath <- liftIO $ findOrCreatePageJSON pageName
    file filePath

editPage :: ActionM()
editPage = do
    page <- Scotty.param "page"
    body <- Scotty.body

    filePath <- liftIO $ editPageJSON page (Char8.unpack body)
    file filePath

searchMusics :: ActionM()
searchMusics = do
    query <- Scotty.param "query"
    let url =  "https://api.spotify.com/v1/search?type=track&limit=5&q=" ++ query

    access_token <- liftIO $ getAccessToken
    let opts = defaults & Wreq.header "Authorization" .~ [access_token]

    r <-  liftIO $ Wreq.getWith opts url
    raw  (r ^. responseBody) 

getRecommendation :: ActionM()
getRecommendation = do
    query <- Scotty.param "query"
    let url =  "https://api.spotify.com/v1/recommendations?seed_tracks=" ++ query

    access_token <- liftIO $ getAccessToken
    let opts = defaults & Wreq.header "Authorization" .~ [access_token]

    r <-  liftIO $ Wreq.getWith opts url
    raw  (r ^. responseBody) 