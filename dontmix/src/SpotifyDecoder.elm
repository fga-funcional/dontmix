module SpotifyDecoder exposing (Musics, musicDecoder, recommendedMusicsDecoder, searchedMusicsDecoder)

import Json.Decode as D
import Model exposing (Music)


type alias Musics =
    List Music


idDecoder : D.Decoder String
idDecoder =
    D.field "id" D.string


nameDecoder : D.Decoder String
nameDecoder =
    D.field "name" D.string


albumDecoder : D.Decoder String
albumDecoder =
    D.at [ "album", "name" ] D.string


artistDecoder : D.Decoder String
artistDecoder =
    D.field "artists" (D.index 0 (D.field "name" D.string))


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


searchedMusicsDecoder : D.Decoder Musics
searchedMusicsDecoder =
    D.at [ "tracks", "items" ] (D.list musicDecoder)


recommendedMusicsDecoder : D.Decoder Musics
recommendedMusicsDecoder =
    D.field "tracks" (D.list musicDecoder)
