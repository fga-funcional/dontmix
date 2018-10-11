module SpotifyDecoder exposing (Music, decode, example, musicDecoder)

import Json.Decode as D


type alias Music =
    { name : String
    , artist : String
    , album : String
    , duration : Int
    }


example =
    """
{
  "tracks" : {
    "href" : "https://api.spotify.com/v1/search?query=hurt&type=track&market=BR&offset=0&limit=1",
    "items" : [ {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/6kACVPfCOnqzgfEF5ryl0x"
          },
          "href" : "https://api.spotify.com/v1/artists/6kACVPfCOnqzgfEF5ryl0x",
          "id" : "6kACVPfCOnqzgfEF5ryl0x",
          "name" : "Johnny Cash",
          "type" : "artist",
          "uri" : "spotify:artist:6kACVPfCOnqzgfEF5ryl0x"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/2BlL4Gv2DLPu8p58Wcmlm9"
        },
        "href" : "https://api.spotify.com/v1/albums/2BlL4Gv2DLPu8p58Wcmlm9",
        "id" : "2BlL4Gv2DLPu8p58Wcmlm9",
        "images" : [ {
          "height" : 638,
          "url" : "https://i.scdn.co/image/caa334a497de0d0c321a28f4861f79d42ce4f9e9",
          "width" : 640
        }, {
          "height" : 299,
          "url" : "https://i.scdn.co/image/d63eff952713141283ade1f3d185deed4916ed80",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/e44622c420dce0d24d78203048c1fc96e1649480",
          "width" : 64
        } ],
        "name" : "American IV: The Man Comes Around",
        "release_date" : "2002-01-01",
        "release_date_precision" : "day",
        "total_tracks" : 15,
        "type" : "album",
        "uri" : "spotify:album:2BlL4Gv2DLPu8p58Wcmlm9"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/6kACVPfCOnqzgfEF5ryl0x"
        },
        "href" : "https://api.spotify.com/v1/artists/6kACVPfCOnqzgfEF5ryl0x",
        "id" : "6kACVPfCOnqzgfEF5ryl0x",
        "name" : "Johnny Cash",
        "type" : "artist",
        "uri" : "spotify:artist:6kACVPfCOnqzgfEF5ryl0x"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 216533,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "USDJ20200650"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/28cnXtME493VX9NOw9cIUh"
      },
      "href" : "https://api.spotify.com/v1/tracks/28cnXtME493VX9NOw9cIUh",
      "id" : "28cnXtME493VX9NOw9cIUh",
      "is_local" : false,
      "name" : "Hurt",
      "popularity" : 76,
      "preview_url" : null,
      "track_number" : 2,
      "type" : "track",
      "uri" : "spotify:track:28cnXtME493VX9NOw9cIUh"
    } ],
    "limit" : 1,
    "next" : "https://api.spotify.com/v1/search?query=hurt&type=track&market=BR&offset=1&limit=1",
    "offset" : 0,
    "previous" : null,
    "total" : 23195
  }
}
"""


musicDecoder : D.Decoder Music
musicDecoder =
    D.map4 Music
        (D.at [ "tracks", "items" ] (D.index 0 (D.field "name" D.string)))
        (D.at [ "tracks", "items" ] (D.index 0 (D.field "artists" (D.index 0 (D.field "name" D.string)))))
        (D.at [ "tracks", "items" ] (D.index 0 (D.at [ "album", "name" ] D.string)))
        (D.at [ "tracks", "items" ] (D.index 0 (D.field "duration_ms" D.int)))


decode =
    let
        decodeHelp =
            D.decodeString musicDecoder example
    in
    case decodeHelp of
        Ok m ->
            m

        Err _ ->
            { name = "", artist = "", album = "", duration = 0 }
