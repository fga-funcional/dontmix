module App exposing (init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import SpotifyDecoder as D


main =
    Browser.element
        { view = view
        , update = update
        , subscriptions = subscriptions
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
    , searchedMusics : List Music
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { musics = []
      , input = ""
      , searchedMusics = []
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS -----------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MESSAGES ----------------------------------------------


type Msg
    = Add (List Music)
    | Save String
    | SaveSearchedMusic (Result Http.Error (List Music))
    | Search


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Add musics ->
            ( { m | musics = musics ++ m.musics }, Cmd.none )

        Save input ->
            ( { m | input = input }, Cmd.none )

        Search ->
            ( m, searchMusic m.input )

        SaveSearchedMusic result ->
            case result of
                Ok searchedMusics ->
                    ( { m | searchedMusics = searchedMusics }, Cmd.none )

                Err _ ->
                    ( { m | searchedMusics = [] }
                    , Cmd.none
                    )



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text "Dontmix" ]
        , input [ placeholder "Type here", onInput Save ] []
        , button [ onClick <| Search ] [ text "Search Music" ]
        , div [ style "display" "flex" ]
            [ showPlaylist m.searchedMusics
            , showSavedMusics m.musics
            ]
        ]


showPlaylist : List Music -> Html Msg
showPlaylist playlist =
    let
        showMusic music =
            li ([ onClick <| Add [ music ], style "cursor" "pointer" ] ++ liAttributes)
                [ p []
                    [ b [] [ text "Song: " ], text music.name ]
                , p []
                    [ b [] [ text "Artist: " ], text music.artist ]
                , p []
                    [ b [] [ text "Album: " ], text music.album ]
                , p []
                    [ b [] [ text "Duration (ms): " ], text (String.fromInt music.duration) ]
                ]

        musics =
            List.map showMusic playlist
    in
    div []
        [ h2 [] [ text "Search Results" ]
        , ul (musicListAttributes ++ [ style "list-style" "none", style "padding" "0" ]) musics
        ]


showSavedMusics : List Music -> Html Msg
showSavedMusics playlist =
    let
        showMusic music =
            li liAttributes
                [ p []
                    [ b [] [ text "Song: " ], text music.name ]
                , p []
                    [ b [] [ text "Artist: " ], text music.artist ]
                , p []
                    [ b [] [ text "Album: " ], text music.album ]
                , p []
                    [ b [] [ text "Duration (ms): " ], text (String.fromInt music.duration) ]
                ]

        musics =
            List.map showMusic playlist
    in
    div []
        [ h2 [] [ text "My Playlist" ]
        , ol musicListAttributes musics
        ]


searchMusic : String -> Cmd Msg
searchMusic query =
    Http.send SaveSearchedMusic (searchRequest query)


searchRequest : String -> Http.Request (List Music)
searchRequest query =
    Http.get ("localhost:3000/search/" ++ query) D.musicsDecoder


-- CSS --------------------------------------------------


liAttributes =
    [ style "padding" "5px", style "border-bottom" "1px solid grey" ]


musicListAttributes =
    [ style "max-height" "450px"
    , style "overflow-y" "scroll"
    , style "min-width" "300px"
    ]
