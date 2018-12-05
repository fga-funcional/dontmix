module APIHandler exposing (encodeMusics)

import Json.Encode as Encode
import Model exposing (Model, Music)


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
