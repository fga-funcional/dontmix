module App exposing (init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox
        { view = view
        , update = update
        , init = init
        }



-- MODEL --------------------------------------------------


type alias Music =
    { name : String
    , artist : String
    , album : String
    , duration : Int
    }


type alias Model =
    List Music


init : Model
init =
    [ { name = "Music 1", artist = "", album = "", duration = 0 }
    , { name = "Music 2", artist = "", album = "", duration = 0 }
    ]



-- MESSAGES ----------------------------------------------


type Msg
    = Add Music


update : Msg -> Model -> Model
update msg m =
    case msg of
        Add music ->
            music :: m



--     case msg of
--         Clear ->
--             ""
--         Concat st ->
--             m ++ " " ++ st
--         Prepend st ->
--             st ++ " " ++ m
--         Replace st ->
--             st
--         NoOp ->
--             m
-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ showPlaylist m ]


showPlaylist : List Music -> Html Msg
showPlaylist playlist =
    let
        showMusic music =
            li [] [ h1 [] [ text music.name ] ]

        musics =
            List.map showMusic playlist
    in
    ul [] musics
