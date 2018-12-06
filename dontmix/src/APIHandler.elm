module APIHandler exposing (Page, encodeMusics, pageDecoder)

import Json.Decode as D
import Json.Encode as Encode
import Model exposing (Model, Music)


encodeMusics : Model -> Encode.Value
encodeMusics lst =
    Encode.object
        [ ( "Selected", Encode.list musicEncoder lst.musics )
        , ( "Recommended", Encode.list musicEncoder lst.recommendedMusics )
        ]


musicEncoder : Music -> Encode.Value
musicEncoder music =
    Encode.object
        [ ( "id", Encode.string music.id )
        , ( "name", Encode.string music.name )
        , ( "artist", Encode.string music.artist )
        , ( "album", Encode.string music.album )
        , ( "duration_ms", Encode.int music.duration )
        ]


idDecoder : D.Decoder String
idDecoder =
    D.field "id" D.string


nameDecoder : D.Decoder String
nameDecoder =
    D.field "name" D.string


albumDecoder : D.Decoder String
albumDecoder =
    D.field "album" D.string


artistDecoder : D.Decoder String
artistDecoder =
    D.field "artist" D.string


durationDecoder : D.Decoder Int
durationDecoder =
    D.field "duration_ms" D.int


musicDecoder : D.Decoder Music
musicDecoder =
    D.map5 Music
        idDecoder
        nameDecoder
        artistDecoder
        albumDecoder
        durationDecoder


recommendedMusicsDecoder : D.Decoder (List Music)
recommendedMusicsDecoder =
    D.field "Recommended" (D.list musicDecoder)


selectedMusicsDecoder : D.Decoder (List Music)
selectedMusicsDecoder =
    D.field "Selected" (D.list musicDecoder)


type alias Page =
    { selected : List Music, recommended : List Music }


pageDecoder : D.Decoder Page
pageDecoder =
    D.map2 Page
        selectedMusicsDecoder
        recommendedMusicsDecoder
