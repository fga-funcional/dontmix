module Msg exposing (Msg(..), update)

import APIHandler exposing (encodeMusics)
import Browser
import Browser.Navigation as Nav
import Http
import Model exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
import SpotifyDecoder as D
import Url



-- MESSAGES ----------------------------------------------


type Msg
    = Add (List Music)
    | Save String
    | SaveSearchedMusic (Result Http.Error (List Music))
    | Search
    | RecommendMusics
    | SaveRecommendMusics (Result Http.Error (List Music))
    | ShuffledList (List Music)
    | ShuffleIt
    | UrlChanged Url.Url
    | UrlRequest Browser.UrlRequest


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

        ShuffleIt ->
            ( m, generate ShuffledList (shuffle m.musics) )

        ShuffledList list ->
            ( { m | musics = list }, Cmd.none )

        UrlChanged url ->
            ( { m | url = Debug.log "HUE" url }
            , Nav.pushUrl m.key (Url.toString url)
            )

        UrlRequest _ ->
            ( m, Cmd.none )


searchMusic : String -> Cmd Msg
searchMusic query =
    Http.send SaveSearchedMusic (searchRequest query)


postPage : Model -> Cmd Msg
postPage m =
    Http.post
        { url = "http://localhost:3000/books"
        , body = jsonBody (encodeMusics m)
        , expect = Http.expectJson GotBooks (list string)
        }


recommendMusics : List Music -> Cmd Msg
recommendMusics selectedMusics =
    Http.send SaveRecommendMusics (recommendationRequest (List.map getMusicId (List.take 5 selectedMusics)))


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
    music.id
