module App exposing (init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SpotifyDecoder as D


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
    { musics : List Music
    , input : String
    }


init : Model
init =
    { musics =
        [ { name = "Music 1", artist = "", album = "", duration = 0 }
        , { name = "Music 2", artist = "", album = "", duration = 0 }
        ]
    , input = ""
    }



-- MESSAGES ----------------------------------------------


type Msg
    = Add (List Music)
    | Save String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Add musics ->
            { m | musics = musics ++ m.musics }

        Save input ->
            { m | input = input }



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ showPlaylist m.musics
        , input [ placeholder "Type here", onInput Save ] []
        , button [ onClick (Add D.decode) ] [ text "Add Music" ]
        ]


showPlaylist : List Music -> Html Msg
showPlaylist playlist =
    let
        showMusic music =
            li []
                [ h1 []
                    [ text music.name
                    , text music.artist
                    , text music.album
                    , text (String.fromInt music.duration)
                    ]
                ]

        musics =
            List.map showMusic playlist
    in
    ul [] musics
