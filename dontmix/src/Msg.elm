module Msg exposing (Msg(..), getPage, init, update)

import APIHandler exposing (Page, encodeMusics, pageDecoder)
import Browser
import Browser.Navigation as Nav
import Http
import Model exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
import SpotifyDecoder as D
import Url



-- MESSAGES ----------------------------------------------


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { musics = []
      , input = ""
      , searchedMusics = []
      , recommendedMusics = []
      , url = url
      , key = key
      }
    , getPage url.path
    )


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
    | GetPage (Result Http.Error Page)
    | SavePage
    | Noop (Result Http.Error ())


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
            ( { m | url = url }
            , Cmd.none
            )

        UrlRequest _ ->
            ( m, Cmd.none )

        GetPage result ->
            case result of
                Ok page ->
                    ( { m | recommendedMusics = page.recommended, musics = page.selected }, Cmd.none )

                Err _ ->
                    ( m, Cmd.none )

        SavePage ->
            ( m, savePage m )

        Noop _ ->
            ( m, Cmd.none )


savePage : Model -> Cmd Msg
savePage model =
    Http.send GetPage (postPage model)


postPage : Model -> Http.Request Page
postPage m =
    Http.post ("http://localhost:8080/api" ++ m.url.path) (Http.jsonBody (encodeMusics m)) pageDecoder


getPage : String -> Cmd Msg
getPage query =
    Http.send GetPage (getPageRequest query)


getPageRequest : String -> Http.Request Page
getPageRequest query =
    Http.get ("http://localhost:8080/api" ++ query) pageDecoder


searchMusic : String -> Cmd Msg
searchMusic query =
    Http.send SaveSearchedMusic (searchRequest query)


searchRequest : String -> Http.Request (List Music)
searchRequest query =
    Http.get ("http://localhost:8080/api/search/" ++ query) D.searchedMusicsDecoder


recommendMusics : List Music -> Cmd Msg
recommendMusics selectedMusics =
    Http.send SaveRecommendMusics (recommendationRequest (List.map getMusicId (List.take 5 selectedMusics)))


recommendationRequest : List String -> Http.Request (List Music)
recommendationRequest query =
    let
        seed =
            String.join "%2C" query
    in
    Http.get ("http://localhost:8080/api/recommendation/" ++ seed) D.recommendedMusicsDecoder


getMusicId : Music -> String
getMusicId music =
    music.id
