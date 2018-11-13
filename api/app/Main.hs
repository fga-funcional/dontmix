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
-- import Data.ByteString.Char8 as BS

main :: IO ()
main = scotty 3000 $
    S.get "/search/:query" $ do
        query <- S.param "query"
        
        text $ (fmap (Data.Text.Encoding.decodeUtf8)  (getMusic query))


-- getMusic :: [Char] -> IO (ByteString) 
getMusic query = 
    do
        let url =  "https://api.spotify.com/v1/search?type=track&limit=5&q=" ++ query
        let opts = defaults & W.header "Authorization" .~ ["Bearer BQBTXcbp9gptikt3q9lHiI36oZUQUQwb3geIj4h0ECoOuCHuxJp73WiLQEFri7YOyzYvRMwyEGjbRo7XDDFERHjUgRFMvArJ4uwApdFXLoa-2lvfWq6kZMCcMzOJarKKq_rhWE_YXeNXdS6XG64"]
        
        r <- W.getWith opts url
        -- r <- getWith opts url
        return $ (r ^. responseBody) 


