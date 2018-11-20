module Msg exposing (Msg(..), update)

import Http
import Model exposing (..)
import SpotifyDecoder as D



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


searchMusic : String -> Cmd Msg
searchMusic query =
    Http.send SaveSearchedMusic (searchRequest query)


searchRequest : String -> Http.Request (List Music)
searchRequest query =
    -- let
    --     headers =
    --         [ Http.header "Authorization" "Bearer BQAEQhFzboT5A5Gdc0lExBQZPGaU7M_IX5pSbbPh4ynkhUd0HdPxSxACwedJgz7mtZ33JV7QtHlAmNyan9V9MHqpzY4gHOrfzuJ26mQTcMsw-4e-DWSxcTlN7o7gxC9eVe_3paXBBdvtw1qSyWw"
    --         , Http.header "Accept" "application/json"
    --         , Http.header "Content-Type" "application/json"
    --         ]
    --     url =
    --         "https://api.spotify.com/v1/search?type=track&limit=5&q=" ++ query
    -- in
    -- Http.request
    --     { method = "GET"
    --     , headers = headers
    --     , url = url
    --     , body = Http.emptyBody
    --     , expect = Http.expectJson D.musicsDecoder
    --     , timeout = Nothing
    --     , withCredentials = False
    --     }
    Http.get ("http://localhost:3000/search/" ++ query) D.musicsDecoder
