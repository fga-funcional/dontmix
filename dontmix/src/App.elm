module App exposing (main)

import Browser
import Model exposing (..)
import Msg exposing (..)
import View exposing (..)


main =
    Browser.element
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }



-- SUBSCRIPTIONS -----------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
