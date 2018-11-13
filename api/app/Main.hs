{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Web.Scotty
import Data.Monoid (mconcat)

main :: IO ()
main = scotty 3000 $
    get "/:page" $ do
        beam <- param "page"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]