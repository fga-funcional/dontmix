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
                    ( {m | searchedMusics = []}
                    , Cmd.none
                    )



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ showSavedMusics m.musics
        , input [ placeholder "Type here", onInput Save ] []
        , button [ onClick <| Search ] [ text "Search Music" ]
        , showPlaylist m.searchedMusics
        ]


showPlaylist : List Music -> Html Msg
showPlaylist playlist =
    let
        showMusic music =
            li [ onClick <| Add [ music ], style "cursor" "pointer" ]
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


showSavedMusics : List Music -> Html Msg
showSavedMusics playlist =
    let
        showMusic music =
            li []
                [ p []
                    [ b [] [text "Song: "] ,text music.name ]
                , p []
                    [ b [] [text "Artist: "] , text music.artist ]
                , p []
                    [  b [] [text "Album: "] , text music.album ]
                , p []
                    [  b [] [text "Duration (ms): "] , text (String.fromInt music.duration) ]
                ]

        musics =
            List.map showMusic playlist
    in
    ol [] musics


searchMusic : String -> Cmd Msg
searchMusic query =
    Http.send SaveSearchedMusic (searchRequest query)


searchRequest : String -> Http.Request (List Music)
searchRequest query =
    let
        headers =
            [ Http.header "Authorization" "Bearer BQATZ5YTV6cG6158GTSOFFI2FstKEtvpfbZzMPGU1EVKX1yr0VJARB62rp0NwqY4hn9aAn7wYA1v4vYKt2pXSZoQAOZI3XaCqkYvE07um88eK4z14yScxuvrC_t10eyKlKIGnUXCXeTPAc6X2sM"
            , Http.header "Accept" "application/json"
            , Http.header "Content-Type" "application/json"
            ]

        url =
            "https://api.spotify.com/v1/search?type=track&limit=5&q=" ++ query
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson D.musicsDecoder
        , timeout = Nothing
        , withCredentials = False
        }
