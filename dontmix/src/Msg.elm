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
    | RecommendMusics
    | SaveRecommendMusics (Result Http.Error (List Music))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Add musics ->
            ( { m | musics = musics ++ m.musics }, Cmd.none )

        Save input ->
            ( { m | input = input }, Cmd.none )

        Search ->
            ( m, searchMusic m.input )

        RecommendMusics ->
            ( m, recommendMusics m.musics )

        SaveSearchedMusic result ->
            case result of
                Ok searchedMusics ->
                    ( { m | searchedMusics = searchedMusics }, Cmd.none )

                Err _ ->
                    ( { m | searchedMusics = [] }
                    , Cmd.none
                    )

        SaveRecommendMusics result ->
            case result of
                Ok recommendedMusics ->
                    ( { m | recommendedMusics = recommendedMusics }, Cmd.none )

                Err _ ->
                    ( { m | recommendedMusics = [] }
                    , Cmd.none
                    )


searchMusic : String -> Cmd Msg
searchMusic query =
    Http.send SaveSearchedMusic (searchRequest query)


recommendMusics : List Music -> Cmd Msg
recommendMusics selectedMusics =
    Http.send SaveRecommendMusics (recommendationRequest (List.map getMusicId selectedMusics))


searchRequest : String -> Http.Request (List Music)
searchRequest query =
    Http.get ("http://localhost:3000/search/" ++ query) D.searchedMusicsDecoder


recommendationRequest : List String -> Http.Request (List Music)
recommendationRequest query =
    let
        seed =
            String.join "%2C" query
    in
    Http.get ("http://localhost:3000/recommendation/" ++ seed) D.recommendedMusicsDecoder


getMusicId : Music -> String
getMusicId music =
    music.name
