module APIHandler exposing (encodeMusics)

import Json.Decode as D
import Json.Encode as Encode
import Model exposing (Model, Music)
import SpotifyDecoder as SD exposing (musicDecoder)


encodeMusics : Model -> Encode.Value
encodeMusics lst =
    Encode.object
        [ ( "Selected", Encode.list musicEncoder lst.musics )
        , ( "Recommended", Encode.list musicEncoder lst.recommendedMusics )
        ]



-- musicEncoder : D.Encoder Music


musicEncoder : Music -> Encode.Value
musicEncoder music =
    Encode.object
        [ ( "id", Encode.string music.id )
        , ( "name", Encode.string music.name )
        , ( "artist", Encode.string music.artist )
        , ( "album", Encode.string music.album )
        , ( "duration", Encode.int music.duration )
        ]


recommendedMusicsDecoder : D.Decoder SD.Musics
recommendedMusicsDecoder =
    D.field "Recommended" (D.list musicDecoder)


selectedMusicsDecoder : D.Decoder SD.Musics
selectedMusicsDecoder =
    D.field "Selected" (D.list musicDecoder)
