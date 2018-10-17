module SpotifyDecoder exposing (Music, decode, example, musicDecoder)

import Json.Decode as D


type alias Music =
    { name : String
    , artist : String
    , album : String
    , duration : Int
    }


type alias Musics =
    List Music


example =
    """
{
  "tracks" : {
    "href" : "https://api.spotify.com/v1/search?query=hurt&type=track&market=BR&offset=0&limit=20",
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
    }, {
      "album" : {
        "album_type" : "single",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/1Xyo4u8uXC1ZmMpatF05PJ"
          },
          "href" : "https://api.spotify.com/v1/artists/1Xyo4u8uXC1ZmMpatF05PJ",
          "id" : "1Xyo4u8uXC1ZmMpatF05PJ",
          "name" : "The Weeknd",
          "type" : "artist",
          "uri" : "spotify:artist:1Xyo4u8uXC1ZmMpatF05PJ"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/3N88bRVAwQrtKqSV0UgU69"
        },
        "href" : "https://api.spotify.com/v1/albums/3N88bRVAwQrtKqSV0UgU69",
        "id" : "3N88bRVAwQrtKqSV0UgU69",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/9a1ab63b2e11e622d07f2cd89f33c976743f1fd5",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/d79b68937edf5a273ba8edbf84736fd2039e432f",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/10b66d571acc59d5ca655956f36f58f2cefb9d4d",
          "width" : 64
        } ],
        "name" : "My Dear Melancholy,",
        "release_date" : "2018-03-30",
        "release_date_precision" : "day",
        "total_tracks" : 6,
        "type" : "album",
        "uri" : "spotify:album:3N88bRVAwQrtKqSV0UgU69"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/1Xyo4u8uXC1ZmMpatF05PJ"
        },
        "href" : "https://api.spotify.com/v1/artists/1Xyo4u8uXC1ZmMpatF05PJ",
        "id" : "1Xyo4u8uXC1ZmMpatF05PJ",
        "name" : "The Weeknd",
        "type" : "artist",
        "uri" : "spotify:artist:1Xyo4u8uXC1ZmMpatF05PJ"
      }, {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/3hteYQFiMFbJY7wS0xDymP"
        },
        "href" : "https://api.spotify.com/v1/artists/3hteYQFiMFbJY7wS0xDymP",
        "id" : "3hteYQFiMFbJY7wS0xDymP",
        "name" : "Gesaffelstein",
        "type" : "artist",
        "uri" : "spotify:artist:3hteYQFiMFbJY7wS0xDymP"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 230026,
      "explicit" : true,
      "external_ids" : {
        "isrc" : "USUG11800566"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/13wNEvWmBsWaWVtiLPRIW5"
      },
      "href" : "https://api.spotify.com/v1/tracks/13wNEvWmBsWaWVtiLPRIW5",
      "id" : "13wNEvWmBsWaWVtiLPRIW5",
      "is_local" : false,
      "name" : "Hurt You",
      "popularity" : 77,
      "preview_url" : null,
      "track_number" : 5,
      "type" : "track",
      "uri" : "spotify:track:13wNEvWmBsWaWVtiLPRIW5"
    }, {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/4LLpKhyESsyAXpc4laK94U"
          },
          "href" : "https://api.spotify.com/v1/artists/4LLpKhyESsyAXpc4laK94U",
          "id" : "4LLpKhyESsyAXpc4laK94U",
          "name" : "Mac Miller",
          "type" : "artist",
          "uri" : "spotify:artist:4LLpKhyESsyAXpc4laK94U"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/5wtE5aLX5r7jOosmPhJhhk"
        },
        "href" : "https://api.spotify.com/v1/albums/5wtE5aLX5r7jOosmPhJhhk",
        "id" : "5wtE5aLX5r7jOosmPhJhhk",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/00f1b797cf233069db9eec18683cee4a47d95171",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/6dd91ca36a8dd29918947232b682f970a6fada36",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/c7d0595212796c5f3e9e5a4f26be291af0ab6b40",
          "width" : 64
        } ],
        "name" : "Swimming",
        "release_date" : "2018-08-03",
        "release_date_precision" : "day",
        "total_tracks" : 13,
        "type" : "album",
        "uri" : "spotify:album:5wtE5aLX5r7jOosmPhJhhk"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/4LLpKhyESsyAXpc4laK94U"
        },
        "href" : "https://api.spotify.com/v1/artists/4LLpKhyESsyAXpc4laK94U",
        "id" : "4LLpKhyESsyAXpc4laK94U",
        "name" : "Mac Miller",
        "type" : "artist",
        "uri" : "spotify:artist:4LLpKhyESsyAXpc4laK94U"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 245640,
      "explicit" : true,
      "external_ids" : {
        "isrc" : "USWB11801212"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/5p7GiBZNL1afJJDUrOA6C8"
      },
      "href" : "https://api.spotify.com/v1/tracks/5p7GiBZNL1afJJDUrOA6C8",
      "id" : "5p7GiBZNL1afJJDUrOA6C8",
      "is_local" : false,
      "name" : "Hurt Feelings",
      "popularity" : 82,
      "preview_url" : "https://p.scdn.co/mp3-preview/3f3bb12e4f55cd06cdc3829a1f7f02bd11616422?cid=774b29d4f13844c495f206cafdad9c86",
      "track_number" : 2,
      "type" : "track",
      "uri" : "spotify:track:5p7GiBZNL1afJJDUrOA6C8"
    }, {
      "album" : {
        "album_type" : "single",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/2RQXRUsr4IW1f3mKyKsy4B"
          },
          "href" : "https://api.spotify.com/v1/artists/2RQXRUsr4IW1f3mKyKsy4B",
          "id" : "2RQXRUsr4IW1f3mKyKsy4B",
          "name" : "Noah Kahan",
          "type" : "artist",
          "uri" : "spotify:artist:2RQXRUsr4IW1f3mKyKsy4B"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/1TMA2dKLdsJZ8u1iikE6Ow"
        },
        "href" : "https://api.spotify.com/v1/albums/1TMA2dKLdsJZ8u1iikE6Ow",
        "id" : "1TMA2dKLdsJZ8u1iikE6Ow",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/29378901ea98067fc07466fa9b39dcfb74354138",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/b994dfdb5523c78e7992a0b1d2369ddeeadea574",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/e5428ae83711970f66ee2ab3872772c912de6d89",
          "width" : 64
        } ],
        "name" : "Hurt Somebody",
        "release_date" : "2018-02-02",
        "release_date_precision" : "day",
        "total_tracks" : 5,
        "type" : "album",
        "uri" : "spotify:album:1TMA2dKLdsJZ8u1iikE6Ow"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/2RQXRUsr4IW1f3mKyKsy4B"
        },
        "href" : "https://api.spotify.com/v1/artists/2RQXRUsr4IW1f3mKyKsy4B",
        "id" : "2RQXRUsr4IW1f3mKyKsy4B",
        "name" : "Noah Kahan",
        "type" : "artist",
        "uri" : "spotify:artist:2RQXRUsr4IW1f3mKyKsy4B"
      }, {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/0ZED1XzwlLHW4ZaG4lOT6m"
        },
        "href" : "https://api.spotify.com/v1/artists/0ZED1XzwlLHW4ZaG4lOT6m",
        "id" : "0ZED1XzwlLHW4ZaG4lOT6m",
        "name" : "Julia Michaels",
        "type" : "artist",
        "uri" : "spotify:artist:0ZED1XzwlLHW4ZaG4lOT6m"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 168640,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "USUM71800031"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/7vA2Y79Q4bBqdzBCfHeGEe"
      },
      "href" : "https://api.spotify.com/v1/tracks/7vA2Y79Q4bBqdzBCfHeGEe",
      "id" : "7vA2Y79Q4bBqdzBCfHeGEe",
      "is_local" : false,
      "name" : "Hurt Somebody (With Julia Michaels)",
      "popularity" : 81,
      "preview_url" : null,
      "track_number" : 1,
      "type" : "track",
      "uri" : "spotify:track:7vA2Y79Q4bBqdzBCfHeGEe"
    }, {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/5sWHDYs0csV6RS48xBl0tH"
          },
          "href" : "https://api.spotify.com/v1/artists/5sWHDYs0csV6RS48xBl0tH",
          "id" : "5sWHDYs0csV6RS48xBl0tH",
          "name" : "Two Feet",
          "type" : "artist",
          "uri" : "spotify:artist:5sWHDYs0csV6RS48xBl0tH"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/4ThJUigPBlok72LoKGk2g1"
        },
        "href" : "https://api.spotify.com/v1/albums/4ThJUigPBlok72LoKGk2g1",
        "id" : "4ThJUigPBlok72LoKGk2g1",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/cc8a9a843a0e27d08178ae1010180ed63fc7cbdd",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/5e57665b2a8fe3cea502d0f5815003e1a2727429",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/f75f14d4c674d5749a31cd025d175419586df72a",
          "width" : 64
        } ],
        "name" : "A 20 Something Fuck",
        "release_date" : "2018-10-05",
        "release_date_precision" : "day",
        "total_tracks" : 8,
        "type" : "album",
        "uri" : "spotify:album:4ThJUigPBlok72LoKGk2g1"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/5sWHDYs0csV6RS48xBl0tH"
        },
        "href" : "https://api.spotify.com/v1/artists/5sWHDYs0csV6RS48xBl0tH",
        "id" : "5sWHDYs0csV6RS48xBl0tH",
        "name" : "Two Feet",
        "type" : "artist",
        "uri" : "spotify:artist:5sWHDYs0csV6RS48xBl0tH"
      }, {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/3BkE65DVH2NZSDQa6ZszcJ"
        },
        "href" : "https://api.spotify.com/v1/artists/3BkE65DVH2NZSDQa6ZszcJ",
        "id" : "3BkE65DVH2NZSDQa6ZszcJ",
        "name" : "Madison Love",
        "type" : "artist",
        "uri" : "spotify:artist:3BkE65DVH2NZSDQa6ZszcJ"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 193813,
      "explicit" : true,
      "external_ids" : {
        "isrc" : "USUM71810268"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/16mrdTSWwfVfOiZNw84tQC"
      },
      "href" : "https://api.spotify.com/v1/tracks/16mrdTSWwfVfOiZNw84tQC",
      "id" : "16mrdTSWwfVfOiZNw84tQC",
      "is_local" : false,
      "name" : "Hurt People",
      "popularity" : 59,
      "preview_url" : null,
      "track_number" : 5,
      "type" : "track",
      "uri" : "spotify:track:16mrdTSWwfVfOiZNw84tQC"
    }, {
      "album" : {
        "album_type" : "single",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/1QAJqy2dA3ihHBFIHRphZj"
          },
          "href" : "https://api.spotify.com/v1/artists/1QAJqy2dA3ihHBFIHRphZj",
          "id" : "1QAJqy2dA3ihHBFIHRphZj",
          "name" : "Cigarettes After Sex",
          "type" : "artist",
          "uri" : "spotify:artist:1QAJqy2dA3ihHBFIHRphZj"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/5fzwP4FRJjFQBBmhKJwpWn"
        },
        "href" : "https://api.spotify.com/v1/albums/5fzwP4FRJjFQBBmhKJwpWn",
        "id" : "5fzwP4FRJjFQBBmhKJwpWn",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/47b83b9e4f6018ed8c4ab550d7364f7bf439ebf4",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/4851cf0bdaceff5f2e734992ac72f506ef255fb0",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/f43afcfb9d634898c7e28187b01a1c6eddfe61ae",
          "width" : 64
        } ],
        "name" : "EP I.",
        "release_date" : "2012",
        "release_date_precision" : "year",
        "total_tracks" : 4,
        "type" : "album",
        "uri" : "spotify:album:5fzwP4FRJjFQBBmhKJwpWn"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/1QAJqy2dA3ihHBFIHRphZj"
        },
        "href" : "https://api.spotify.com/v1/artists/1QAJqy2dA3ihHBFIHRphZj",
        "id" : "1QAJqy2dA3ihHBFIHRphZj",
        "name" : "Cigarettes After Sex",
        "type" : "artist",
        "uri" : "spotify:artist:1QAJqy2dA3ihHBFIHRphZj"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 286293,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "TCACJ1593857"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/1W7Eajq8Hlqiy39QnuKjvD"
      },
      "href" : "https://api.spotify.com/v1/tracks/1W7Eajq8Hlqiy39QnuKjvD",
      "id" : "1W7Eajq8Hlqiy39QnuKjvD",
      "is_local" : false,
      "name" : "Nothing's Gonna Hurt You Baby",
      "popularity" : 67,
      "preview_url" : "https://p.scdn.co/mp3-preview/1b251b68588f44084ee6cb276130e223820bd388?cid=774b29d4f13844c495f206cafdad9c86",
      "track_number" : 1,
      "type" : "track",
      "uri" : "spotify:track:1W7Eajq8Hlqiy39QnuKjvD"
    }, {
      "album" : {
        "album_type" : "single",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/5sWHDYs0csV6RS48xBl0tH"
          },
          "href" : "https://api.spotify.com/v1/artists/5sWHDYs0csV6RS48xBl0tH",
          "id" : "5sWHDYs0csV6RS48xBl0tH",
          "name" : "Two Feet",
          "type" : "artist",
          "uri" : "spotify:artist:5sWHDYs0csV6RS48xBl0tH"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/2fZ02HGHiFZFr2Mu6J6fHQ"
        },
        "href" : "https://api.spotify.com/v1/albums/2fZ02HGHiFZFr2Mu6J6fHQ",
        "id" : "2fZ02HGHiFZFr2Mu6J6fHQ",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/627d5beb35fa0d2a58e07526321367f855c3d091",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/578c74e775269b2be51c8f7879b54260c4a68ed6",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/37057f719e10e86c0cef6151b689f5ad2ea2a4e7",
          "width" : 64
        } ],
        "name" : "Hurt People",
        "release_date" : "2018-07-18",
        "release_date_precision" : "day",
        "total_tracks" : 1,
        "type" : "album",
        "uri" : "spotify:album:2fZ02HGHiFZFr2Mu6J6fHQ"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/5sWHDYs0csV6RS48xBl0tH"
        },
        "href" : "https://api.spotify.com/v1/artists/5sWHDYs0csV6RS48xBl0tH",
        "id" : "5sWHDYs0csV6RS48xBl0tH",
        "name" : "Two Feet",
        "type" : "artist",
        "uri" : "spotify:artist:5sWHDYs0csV6RS48xBl0tH"
      }, {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/3BkE65DVH2NZSDQa6ZszcJ"
        },
        "href" : "https://api.spotify.com/v1/artists/3BkE65DVH2NZSDQa6ZszcJ",
        "id" : "3BkE65DVH2NZSDQa6ZszcJ",
        "name" : "Madison Love",
        "type" : "artist",
        "uri" : "spotify:artist:3BkE65DVH2NZSDQa6ZszcJ"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 194853,
      "explicit" : true,
      "external_ids" : {
        "isrc" : "USUM71810268"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/3o4OB9XTVutBdykI4VbXHe"
      },
      "href" : "https://api.spotify.com/v1/tracks/3o4OB9XTVutBdykI4VbXHe",
      "id" : "3o4OB9XTVutBdykI4VbXHe",
      "is_local" : false,
      "name" : "Hurt People",
      "popularity" : 66,
      "preview_url" : null,
      "track_number" : 1,
      "type" : "track",
      "uri" : "spotify:track:3o4OB9XTVutBdykI4VbXHe"
    }, {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/2rblp9fJo16ZPTcKDtlmKW"
          },
          "href" : "https://api.spotify.com/v1/artists/2rblp9fJo16ZPTcKDtlmKW",
          "id" : "2rblp9fJo16ZPTcKDtlmKW",
          "name" : "MC Hammer",
          "type" : "artist",
          "uri" : "spotify:artist:2rblp9fJo16ZPTcKDtlmKW"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/4r1WecJyt5FOhglysp9zhN"
        },
        "href" : "https://api.spotify.com/v1/albums/4r1WecJyt5FOhglysp9zhN",
        "id" : "4r1WecJyt5FOhglysp9zhN",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/9dc4c5c4edde03ad2cbd855039b8fd6e048fd9b6",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/b28b2314bb4af0ab5d7f52b28158eca0776af6d4",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/37a1cebf2e9154f64e37dc26253c3730334cb179",
          "width" : 64
        } ],
        "name" : "Please Hammer Don't Hurt 'Em",
        "release_date" : "1990-02-20",
        "release_date_precision" : "day",
        "total_tracks" : 13,
        "type" : "album",
        "uri" : "spotify:album:4r1WecJyt5FOhglysp9zhN"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/2rblp9fJo16ZPTcKDtlmKW"
        },
        "href" : "https://api.spotify.com/v1/artists/2rblp9fJo16ZPTcKDtlmKW",
        "id" : "2rblp9fJo16ZPTcKDtlmKW",
        "name" : "MC Hammer",
        "type" : "artist",
        "uri" : "spotify:artist:2rblp9fJo16ZPTcKDtlmKW"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 257360,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "USCA29000294"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/1B75hgRqe7A4fwee3g3Wmu"
      },
      "href" : "https://api.spotify.com/v1/tracks/1B75hgRqe7A4fwee3g3Wmu",
      "id" : "1B75hgRqe7A4fwee3g3Wmu",
      "is_local" : false,
      "name" : "U Can't Touch This",
      "popularity" : 74,
      "preview_url" : null,
      "track_number" : 2,
      "type" : "track",
      "uri" : "spotify:track:1B75hgRqe7A4fwee3g3Wmu"
    }, {
      "album" : {
        "album_type" : "single",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/3IpvVrP3VLhruTmnququq7"
          },
          "href" : "https://api.spotify.com/v1/artists/3IpvVrP3VLhruTmnququq7",
          "id" : "3IpvVrP3VLhruTmnququq7",
          "name" : "Mike Williams",
          "type" : "artist",
          "uri" : "spotify:artist:3IpvVrP3VLhruTmnququq7"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/2D2QBvtnhYh8z3cOlKfvwZ"
        },
        "href" : "https://api.spotify.com/v1/albums/2D2QBvtnhYh8z3cOlKfvwZ",
        "id" : "2D2QBvtnhYh8z3cOlKfvwZ",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/8394007572f7466d5a7b784f738f056b48176935",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/3d815ddcefb9645c4c9adf916fe7aae354a28d9d",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/4c42eefa223c007011ce2754bd93fe7f8e769df9",
          "width" : 64
        } ],
        "name" : "Don't Hurt (feat. Brezy)",
        "release_date" : "2017-12-01",
        "release_date_precision" : "day",
        "total_tracks" : 1,
        "type" : "album",
        "uri" : "spotify:album:2D2QBvtnhYh8z3cOlKfvwZ"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/3IpvVrP3VLhruTmnququq7"
        },
        "href" : "https://api.spotify.com/v1/artists/3IpvVrP3VLhruTmnququq7",
        "id" : "3IpvVrP3VLhruTmnququq7",
        "name" : "Mike Williams",
        "type" : "artist",
        "uri" : "spotify:artist:3IpvVrP3VLhruTmnququq7"
      }, {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/13NUtXm6qqGiPcRRBNdZsq"
        },
        "href" : "https://api.spotify.com/v1/artists/13NUtXm6qqGiPcRRBNdZsq",
        "id" : "13NUtXm6qqGiPcRRBNdZsq",
        "name" : "Brezy",
        "type" : "artist",
        "uri" : "spotify:artist:13NUtXm6qqGiPcRRBNdZsq"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 215624,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "NLZ541700123"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/0dfy8LYDzGRC2TMMde0vex"
      },
      "href" : "https://api.spotify.com/v1/tracks/0dfy8LYDzGRC2TMMde0vex",
      "id" : "0dfy8LYDzGRC2TMMde0vex",
      "is_local" : false,
      "name" : "Don't Hurt (feat. Brezy)",
      "popularity" : 67,
      "preview_url" : "https://p.scdn.co/mp3-preview/f4fb481a8ee28b5bf74500e36a91844c4f30ee31?cid=774b29d4f13844c495f206cafdad9c86",
      "track_number" : 1,
      "type" : "track",
      "uri" : "spotify:track:0dfy8LYDzGRC2TMMde0vex"
    }, {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/1l7ZsJRRS8wlW3WfJfPfNS"
          },
          "href" : "https://api.spotify.com/v1/artists/1l7ZsJRRS8wlW3WfJfPfNS",
          "id" : "1l7ZsJRRS8wlW3WfJfPfNS",
          "name" : "Christina Aguilera",
          "type" : "artist",
          "uri" : "spotify:artist:1l7ZsJRRS8wlW3WfJfPfNS"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/0zRJsgzHZUUdk8Rjk6Segd"
        },
        "href" : "https://api.spotify.com/v1/albums/0zRJsgzHZUUdk8Rjk6Segd",
        "id" : "0zRJsgzHZUUdk8Rjk6Segd",
        "images" : [ {
          "height" : 633,
          "url" : "https://i.scdn.co/image/c7462e8ab150cf0178366343101e58ea9695c466",
          "width" : 640
        }, {
          "height" : 297,
          "url" : "https://i.scdn.co/image/6c3899c159c54bb455210e9aa2faed97cbaffbcb",
          "width" : 300
        }, {
          "height" : 63,
          "url" : "https://i.scdn.co/image/af44ec5321d506d64771af935961d70d0a931f4a",
          "width" : 64
        } ],
        "name" : "Back To Basics",
        "release_date" : "2006-08-14",
        "release_date_precision" : "day",
        "total_tracks" : 22,
        "type" : "album",
        "uri" : "spotify:album:0zRJsgzHZUUdk8Rjk6Segd"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/1l7ZsJRRS8wlW3WfJfPfNS"
        },
        "href" : "https://api.spotify.com/v1/artists/1l7ZsJRRS8wlW3WfJfPfNS",
        "id" : "1l7ZsJRRS8wlW3WfJfPfNS",
        "name" : "Christina Aguilera",
        "type" : "artist",
        "uri" : "spotify:artist:1l7ZsJRRS8wlW3WfJfPfNS"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 2,
      "duration_ms" : 243373,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "USRC10600420"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/6gDXbcILAyBa2skSdbUYq7"
      },
      "href" : "https://api.spotify.com/v1/tracks/6gDXbcILAyBa2skSdbUYq7",
      "id" : "6gDXbcILAyBa2skSdbUYq7",
      "is_local" : false,
      "name" : "Hurt",
      "popularity" : 66,
      "preview_url" : "https://p.scdn.co/mp3-preview/faece80540654787ed593c491e9165f9fda5cd90?cid=774b29d4f13844c495f206cafdad9c86",
      "track_number" : 6,
      "type" : "track",
      "uri" : "spotify:track:6gDXbcILAyBa2skSdbUYq7"
    }, {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/5Pwc4xIPtQLFEnJriah9YJ"
          },
          "href" : "https://api.spotify.com/v1/artists/5Pwc4xIPtQLFEnJriah9YJ",
          "id" : "5Pwc4xIPtQLFEnJriah9YJ",
          "name" : "OneRepublic",
          "type" : "artist",
          "uri" : "spotify:artist:5Pwc4xIPtQLFEnJriah9YJ"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/30Ni9qg2MBeeZXybsMOQ7m"
        },
        "href" : "https://api.spotify.com/v1/albums/30Ni9qg2MBeeZXybsMOQ7m",
        "id" : "30Ni9qg2MBeeZXybsMOQ7m",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/3af30ea172ff0db16edfcbcc7b2256896f365460",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/637da8c5287d8534fd80acd9b9fac4968cda31cb",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/403d46d92604acfaa3f47074717e5b239690ef30",
          "width" : 64
        } ],
        "name" : "Oh My My",
        "release_date" : "2016-12-02",
        "release_date_precision" : "day",
        "total_tracks" : 16,
        "type" : "album",
        "uri" : "spotify:album:30Ni9qg2MBeeZXybsMOQ7m"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/5Pwc4xIPtQLFEnJriah9YJ"
        },
        "href" : "https://api.spotify.com/v1/artists/5Pwc4xIPtQLFEnJriah9YJ",
        "id" : "5Pwc4xIPtQLFEnJriah9YJ",
        "name" : "OneRepublic",
        "type" : "artist",
        "uri" : "spotify:artist:5Pwc4xIPtQLFEnJriah9YJ"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 194520,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "USUM71608414"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/05sCp83gcMm1iecYydKJS3"
      },
      "href" : "https://api.spotify.com/v1/tracks/05sCp83gcMm1iecYydKJS3",
      "id" : "05sCp83gcMm1iecYydKJS3",
      "is_local" : false,
      "name" : "Let's Hurt Tonight",
      "popularity" : 68,
      "preview_url" : null,
      "track_number" : 1,
      "type" : "track",
      "uri" : "spotify:track:05sCp83gcMm1iecYydKJS3"
    }, {
      "album" : {
        "album_type" : "compilation",
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
          "spotify" : "https://open.spotify.com/album/05BIC4TZptbiQoF03QhojS"
        },
        "href" : "https://api.spotify.com/v1/albums/05BIC4TZptbiQoF03QhojS",
        "id" : "05BIC4TZptbiQoF03QhojS",
        "images" : [ {
          "height" : 634,
          "url" : "https://i.scdn.co/image/ee72e2a57d5746bd6a0be8b507fbd250d3a9412d",
          "width" : 640
        }, {
          "height" : 297,
          "url" : "https://i.scdn.co/image/a7e343ebff6e2fa9b909cc5fbcc39c1e790b622e",
          "width" : 300
        }, {
          "height" : 63,
          "url" : "https://i.scdn.co/image/0a25d4a1b3d9defb8592c3b35a807df2a6647d2d",
          "width" : 64
        } ],
        "name" : "The Legend Of Johnny Cash",
        "release_date" : "2005-01-01",
        "release_date_precision" : "day",
        "total_tracks" : 21,
        "type" : "album",
        "uri" : "spotify:album:05BIC4TZptbiQoF03QhojS"
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
      "duration_ms" : 218586,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "USDJ20200650"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/5VFzIzR8nACf9Bad4O73f6"
      },
      "href" : "https://api.spotify.com/v1/tracks/5VFzIzR8nACf9Bad4O73f6",
      "id" : "5VFzIzR8nACf9Bad4O73f6",
      "is_local" : false,
      "name" : "Hurt",
      "popularity" : 57,
      "preview_url" : null,
      "track_number" : 21,
      "type" : "track",
      "uri" : "spotify:track:5VFzIzR8nACf9Bad4O73f6"
    }, {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/4fwuXg6XQHfdlOdmw36OHa"
          },
          "href" : "https://api.spotify.com/v1/artists/4fwuXg6XQHfdlOdmw36OHa",
          "id" : "4fwuXg6XQHfdlOdmw36OHa",
          "name" : "Paloma Faith",
          "type" : "artist",
          "uri" : "spotify:artist:4fwuXg6XQHfdlOdmw36OHa"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/3jRG3qOfsSSW3SBdeBiIfC"
        },
        "href" : "https://api.spotify.com/v1/albums/3jRG3qOfsSSW3SBdeBiIfC",
        "id" : "3jRG3qOfsSSW3SBdeBiIfC",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/74503ebc96dbac89687023af35c94390531823ba",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/7fb86a84cf8ee2dda2f0f43cbac63120285abfb8",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/53d6bf6ef71bad9d1ca0bada9bef18b6ae865c55",
          "width" : 64
        } ],
        "name" : "A Perfect Contradiction Outsiders' Edition (Deluxe)",
        "release_date" : "2014-11-07",
        "release_date_precision" : "day",
        "total_tracks" : 24,
        "type" : "album",
        "uri" : "spotify:album:3jRG3qOfsSSW3SBdeBiIfC"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/4fwuXg6XQHfdlOdmw36OHa"
        },
        "href" : "https://api.spotify.com/v1/artists/4fwuXg6XQHfdlOdmw36OHa",
        "id" : "4fwuXg6XQHfdlOdmw36OHa",
        "name" : "Paloma Faith",
        "type" : "artist",
        "uri" : "spotify:artist:4fwuXg6XQHfdlOdmw36OHa"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 232893,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "GBARL1301427"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/62ke5zFUJN6RvtXZgVH0F8"
      },
      "href" : "https://api.spotify.com/v1/tracks/62ke5zFUJN6RvtXZgVH0F8",
      "id" : "62ke5zFUJN6RvtXZgVH0F8",
      "is_local" : false,
      "name" : "Only Love Can Hurt Like This",
      "popularity" : 70,
      "preview_url" : "https://p.scdn.co/mp3-preview/f805f78116a914d8af3d173128ba908c8a5a1a14?cid=774b29d4f13844c495f206cafdad9c86",
      "track_number" : 4,
      "type" : "track",
      "uri" : "spotify:track:62ke5zFUJN6RvtXZgVH0F8"
    }, {
      "album" : {
        "album_type" : "single",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/20wkVLutqVOYrc0kxFs7rA"
          },
          "href" : "https://api.spotify.com/v1/artists/20wkVLutqVOYrc0kxFs7rA",
          "id" : "20wkVLutqVOYrc0kxFs7rA",
          "name" : "Daniel Caesar",
          "type" : "artist",
          "uri" : "spotify:artist:20wkVLutqVOYrc0kxFs7rA"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/28laeIqUtWZ2iIZmnWLdmJ"
        },
        "href" : "https://api.spotify.com/v1/albums/28laeIqUtWZ2iIZmnWLdmJ",
        "id" : "28laeIqUtWZ2iIZmnWLdmJ",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/5b47867fa676aa9bc59d84e4ccf0c44ebe9bbe87",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/5b234c3771728849a136fdb2d50adb7d6e6c0606",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/8a091832e838768a87f1caab0bdadb29e179b155",
          "width" : 64
        } ],
        "name" : "Who Hurt You?",
        "release_date" : "2018-10-16",
        "release_date_precision" : "day",
        "total_tracks" : 1,
        "type" : "album",
        "uri" : "spotify:album:28laeIqUtWZ2iIZmnWLdmJ"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/20wkVLutqVOYrc0kxFs7rA"
        },
        "href" : "https://api.spotify.com/v1/artists/20wkVLutqVOYrc0kxFs7rA",
        "id" : "20wkVLutqVOYrc0kxFs7rA",
        "name" : "Daniel Caesar",
        "type" : "artist",
        "uri" : "spotify:artist:20wkVLutqVOYrc0kxFs7rA"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 231964,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "TCADX1816901"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/4CdnRTuTadXDbZDnwvXVkb"
      },
      "href" : "https://api.spotify.com/v1/tracks/4CdnRTuTadXDbZDnwvXVkb",
      "id" : "4CdnRTuTadXDbZDnwvXVkb",
      "is_local" : false,
      "name" : "Who Hurt You?",
      "popularity" : 0,
      "preview_url" : "https://p.scdn.co/mp3-preview/b1cc22b61b184b0b8a6c50e1e8a5a0d0495dcbd3?cid=774b29d4f13844c495f206cafdad9c86",
      "track_number" : 1,
      "type" : "track",
      "uri" : "spotify:track:4CdnRTuTadXDbZDnwvXVkb"
    }, {
      "album" : {
        "album_type" : "single",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/20wkVLutqVOYrc0kxFs7rA"
          },
          "href" : "https://api.spotify.com/v1/artists/20wkVLutqVOYrc0kxFs7rA",
          "id" : "20wkVLutqVOYrc0kxFs7rA",
          "name" : "Daniel Caesar",
          "type" : "artist",
          "uri" : "spotify:artist:20wkVLutqVOYrc0kxFs7rA"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/15M9pZ8gsdoN67yLjyQ039"
        },
        "href" : "https://api.spotify.com/v1/albums/15M9pZ8gsdoN67yLjyQ039",
        "id" : "15M9pZ8gsdoN67yLjyQ039",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/5b47867fa676aa9bc59d84e4ccf0c44ebe9bbe87",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/5b234c3771728849a136fdb2d50adb7d6e6c0606",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/8a091832e838768a87f1caab0bdadb29e179b155",
          "width" : 64
        } ],
        "name" : "Who Hurt You?",
        "release_date" : "2018-10-16",
        "release_date_precision" : "day",
        "total_tracks" : 1,
        "type" : "album",
        "uri" : "spotify:album:15M9pZ8gsdoN67yLjyQ039"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/20wkVLutqVOYrc0kxFs7rA"
        },
        "href" : "https://api.spotify.com/v1/artists/20wkVLutqVOYrc0kxFs7rA",
        "id" : "20wkVLutqVOYrc0kxFs7rA",
        "name" : "Daniel Caesar",
        "type" : "artist",
        "uri" : "spotify:artist:20wkVLutqVOYrc0kxFs7rA"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 231964,
      "explicit" : true,
      "external_ids" : {
        "isrc" : "TCADX1816901"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/23c9gmiiv7RCu7twft0Mym"
      },
      "href" : "https://api.spotify.com/v1/tracks/23c9gmiiv7RCu7twft0Mym",
      "id" : "23c9gmiiv7RCu7twft0Mym",
      "is_local" : false,
      "name" : "Who Hurt You?",
      "popularity" : 0,
      "preview_url" : "https://p.scdn.co/mp3-preview/741bbd449ad8300cbf2d96308ba3a3a8bc001ba2?cid=774b29d4f13844c495f206cafdad9c86",
      "track_number" : 1,
      "type" : "track",
      "uri" : "spotify:track:23c9gmiiv7RCu7twft0Mym"
    }, {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/1oKdM70mJD8VvDOTKeS8t1"
          },
          "href" : "https://api.spotify.com/v1/artists/1oKdM70mJD8VvDOTKeS8t1",
          "id" : "1oKdM70mJD8VvDOTKeS8t1",
          "name" : "Emily Warren",
          "type" : "artist",
          "uri" : "spotify:artist:1oKdM70mJD8VvDOTKeS8t1"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/5J48zeMZUPTxsJFkclmWBx"
        },
        "href" : "https://api.spotify.com/v1/albums/5J48zeMZUPTxsJFkclmWBx",
        "id" : "5J48zeMZUPTxsJFkclmWBx",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/45c39be0c5e9c4046594d851a1760615afb20f28",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/f1bc1915d6ad6aca6b1e7534a925a9e24c1ee78a",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/686c8f4f3345d642270a27ed6d794366c1d0dbb2",
          "width" : 64
        } ],
        "name" : "Quiet Your Mind",
        "release_date" : "2018-10-05",
        "release_date_precision" : "day",
        "total_tracks" : 11,
        "type" : "album",
        "uri" : "spotify:album:5J48zeMZUPTxsJFkclmWBx"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/1oKdM70mJD8VvDOTKeS8t1"
        },
        "href" : "https://api.spotify.com/v1/artists/1oKdM70mJD8VvDOTKeS8t1",
        "id" : "1oKdM70mJD8VvDOTKeS8t1",
        "name" : "Emily Warren",
        "type" : "artist",
        "uri" : "spotify:artist:1oKdM70mJD8VvDOTKeS8t1"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 187493,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "TCADA1783108"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/1ZSPFGKyM0pawbkBxDMPps"
      },
      "href" : "https://api.spotify.com/v1/tracks/1ZSPFGKyM0pawbkBxDMPps",
      "id" : "1ZSPFGKyM0pawbkBxDMPps",
      "is_local" : false,
      "name" : "Hurt By You",
      "popularity" : 35,
      "preview_url" : "https://p.scdn.co/mp3-preview/b6beb261ee15016f8cffe0c4c05c1e2640b5a2d3?cid=774b29d4f13844c495f206cafdad9c86",
      "track_number" : 10,
      "type" : "track",
      "uri" : "spotify:track:1ZSPFGKyM0pawbkBxDMPps"
    }, {
      "album" : {
        "album_type" : "single",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/2Fc1UZXKRmPpWWx1sxcb9m"
          },
          "href" : "https://api.spotify.com/v1/artists/2Fc1UZXKRmPpWWx1sxcb9m",
          "id" : "2Fc1UZXKRmPpWWx1sxcb9m",
          "name" : "SwuM",
          "type" : "artist",
          "uri" : "spotify:artist:2Fc1UZXKRmPpWWx1sxcb9m"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/6HpYdIjvkgVLfoZIXbqoI1"
        },
        "href" : "https://api.spotify.com/v1/albums/6HpYdIjvkgVLfoZIXbqoI1",
        "id" : "6HpYdIjvkgVLfoZIXbqoI1",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/be9fb188ede5fd2776a3aa908f1d1db115288cfa",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/8375dbf0e319599b66ab667c3db267f259c188b5",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/973c980832fdc8776258654b19f7be267e0a771d",
          "width" : 64
        } ],
        "name" : "Nothing's Gonna Hurt U",
        "release_date" : "2018-07-09",
        "release_date_precision" : "day",
        "total_tracks" : 1,
        "type" : "album",
        "uri" : "spotify:album:6HpYdIjvkgVLfoZIXbqoI1"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/2Fc1UZXKRmPpWWx1sxcb9m"
        },
        "href" : "https://api.spotify.com/v1/artists/2Fc1UZXKRmPpWWx1sxcb9m",
        "id" : "2Fc1UZXKRmPpWWx1sxcb9m",
        "name" : "SwuM",
        "type" : "artist",
        "uri" : "spotify:artist:2Fc1UZXKRmPpWWx1sxcb9m"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 171428,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "GBKQU1866168"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/1B0haTCTmju6oUmIJozTvK"
      },
      "href" : "https://api.spotify.com/v1/tracks/1B0haTCTmju6oUmIJozTvK",
      "id" : "1B0haTCTmju6oUmIJozTvK",
      "is_local" : false,
      "name" : "Nothing's Gonna Hurt U",
      "popularity" : 62,
      "preview_url" : "https://p.scdn.co/mp3-preview/3530638f2d1febc7fa8bd403e7798465c1a62009?cid=774b29d4f13844c495f206cafdad9c86",
      "track_number" : 1,
      "type" : "track",
      "uri" : "spotify:track:1B0haTCTmju6oUmIJozTvK"
    }, {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/0X380XXQSNBYuleKzav5UO"
          },
          "href" : "https://api.spotify.com/v1/artists/0X380XXQSNBYuleKzav5UO",
          "id" : "0X380XXQSNBYuleKzav5UO",
          "name" : "Nine Inch Nails",
          "type" : "artist",
          "uri" : "spotify:artist:0X380XXQSNBYuleKzav5UO"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/3i13RcPnJ0AkHEamweLGhq"
        },
        "href" : "https://api.spotify.com/v1/albums/3i13RcPnJ0AkHEamweLGhq",
        "id" : "3i13RcPnJ0AkHEamweLGhq",
        "images" : [ {
          "height" : 539,
          "url" : "https://i.scdn.co/image/3b4b6c358ca5d3c362380fd7a78207a2c693ec29",
          "width" : 600
        }, {
          "height" : 270,
          "url" : "https://i.scdn.co/image/16932592c4ababd673296b78a66bfcd674b366e7",
          "width" : 300
        }, {
          "height" : 57,
          "url" : "https://i.scdn.co/image/62bbe88c73b035d010a709db67fee7e720f0525a",
          "width" : 63
        } ],
        "name" : "The Downward Spiral",
        "release_date" : "1994",
        "release_date_precision" : "year",
        "total_tracks" : 27,
        "type" : "album",
        "uri" : "spotify:album:3i13RcPnJ0AkHEamweLGhq"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/0X380XXQSNBYuleKzav5UO"
        },
        "href" : "https://api.spotify.com/v1/artists/0X380XXQSNBYuleKzav5UO",
        "id" : "0X380XXQSNBYuleKzav5UO",
        "name" : "Nine Inch Nails",
        "type" : "artist",
        "uri" : "spotify:artist:0X380XXQSNBYuleKzav5UO"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 373333,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "USIR19400538"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/4pFnV6aV1bnEDqPDGBlrNp"
      },
      "href" : "https://api.spotify.com/v1/tracks/4pFnV6aV1bnEDqPDGBlrNp",
      "id" : "4pFnV6aV1bnEDqPDGBlrNp",
      "is_local" : false,
      "name" : "Hurt",
      "popularity" : 63,
      "preview_url" : null,
      "track_number" : 14,
      "type" : "track",
      "uri" : "spotify:track:4pFnV6aV1bnEDqPDGBlrNp"
    }, {
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
          "spotify" : "https://open.spotify.com/album/5xeMctXQWNmMq4aAjwuR3e"
        },
        "href" : "https://api.spotify.com/v1/albums/5xeMctXQWNmMq4aAjwuR3e",
        "id" : "5xeMctXQWNmMq4aAjwuR3e",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/fdb45221d842a6ce4fb403d32b11fb9bb5489d6f",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/0e3ae568ee73d4ffbf09e85bb1df2e8c8235c91b",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/4a649ec5094cb039bad99a16c4091207c1b61628",
          "width" : 64
        } ],
        "name" : "Unearthed",
        "release_date" : "2003-01-01",
        "release_date_precision" : "day",
        "total_tracks" : 79,
        "type" : "album",
        "uri" : "spotify:album:5xeMctXQWNmMq4aAjwuR3e"
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
      "disc_number" : 5,
      "duration_ms" : 216533,
      "explicit" : false,
      "external_ids" : {
        "isrc" : "USDJ20200650"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/5rpRzNcJZqKQXk9PIjreB6"
      },
      "href" : "https://api.spotify.com/v1/tracks/5rpRzNcJZqKQXk9PIjreB6",
      "id" : "5rpRzNcJZqKQXk9PIjreB6",
      "is_local" : false,
      "name" : "Hurt",
      "popularity" : 64,
      "preview_url" : null,
      "track_number" : 15,
      "type" : "track",
      "uri" : "spotify:track:5rpRzNcJZqKQXk9PIjreB6"
    }, {
      "album" : {
        "album_type" : "album",
        "artists" : [ {
          "external_urls" : {
            "spotify" : "https://open.spotify.com/artist/4MCBfE4596Uoi2O4DtmEMz"
          },
          "href" : "https://api.spotify.com/v1/artists/4MCBfE4596Uoi2O4DtmEMz",
          "id" : "4MCBfE4596Uoi2O4DtmEMz",
          "name" : "Juice WRLD",
          "type" : "artist",
          "uri" : "spotify:artist:4MCBfE4596Uoi2O4DtmEMz"
        } ],
        "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
        "external_urls" : {
          "spotify" : "https://open.spotify.com/album/2viWIVEItaDlFH2RXWKwS7"
        },
        "href" : "https://api.spotify.com/v1/albums/2viWIVEItaDlFH2RXWKwS7",
        "id" : "2viWIVEItaDlFH2RXWKwS7",
        "images" : [ {
          "height" : 640,
          "url" : "https://i.scdn.co/image/0653b51eb20f46816c1f525da6e0696a6e7262ad",
          "width" : 640
        }, {
          "height" : 300,
          "url" : "https://i.scdn.co/image/19c24900a8d89bd80db1446ab67e403edf921c47",
          "width" : 300
        }, {
          "height" : 64,
          "url" : "https://i.scdn.co/image/e351fde0fb77b8f64ed8883c429cada24306600d",
          "width" : 64
        } ],
        "name" : "Goodbye & Good Riddance",
        "release_date" : "2018-05-23",
        "release_date_precision" : "day",
        "total_tracks" : 16,
        "type" : "album",
        "uri" : "spotify:album:2viWIVEItaDlFH2RXWKwS7"
      },
      "artists" : [ {
        "external_urls" : {
          "spotify" : "https://open.spotify.com/artist/4MCBfE4596Uoi2O4DtmEMz"
        },
        "href" : "https://api.spotify.com/v1/artists/4MCBfE4596Uoi2O4DtmEMz",
        "id" : "4MCBfE4596Uoi2O4DtmEMz",
        "name" : "Juice WRLD",
        "type" : "artist",
        "uri" : "spotify:artist:4MCBfE4596Uoi2O4DtmEMz"
      } ],
      "available_markets" : [ "AD", "AR", "AT", "AU", "BE", "BG", "BO", "BR", "CA", "CH", "CL", "CO", "CR", "CY", "CZ", "DE", "DK", "DO", "EC", "EE", "ES", "FI", "FR", "GB", "GR", "GT", "HK", "HN", "HU", "ID", "IE", "IL", "IS", "IT", "JP", "LI", "LT", "LU", "LV", "MC", "MT", "MX", "MY", "NI", "NL", "NO", "NZ", "PA", "PE", "PH", "PL", "PT", "PY", "RO", "SE", "SG", "SK", "SV", "TH", "TR", "TW", "US", "UY", "VN", "ZA" ],
      "disc_number" : 1,
      "duration_ms" : 122285,
      "explicit" : true,
      "external_ids" : {
        "isrc" : "USUG11800953"
      },
      "external_urls" : {
        "spotify" : "https://open.spotify.com/track/6feFJnZ7UZwDzqrllDfRkS"
      },
      "href" : "https://api.spotify.com/v1/tracks/6feFJnZ7UZwDzqrllDfRkS",
      "id" : "6feFJnZ7UZwDzqrllDfRkS",
      "is_local" : false,
      "name" : "Hurt Me",
      "popularity" : 79,
      "preview_url" : null,
      "track_number" : 12,
      "type" : "track",
      "uri" : "spotify:track:6feFJnZ7UZwDzqrllDfRkS"
    } ],
    "limit" : 20,
    "next" : "https://api.spotify.com/v1/search?query=hurt&type=track&market=BR&offset=20&limit=20",
    "offset" : 0,
    "previous" : null,
    "total" : 23249
  }
}
"""


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
    D.map4 Music
        nameDecoder
        artistDecoder
        albumDecoder
        durationDecoder


musicsDecoder : D.Decoder Musics
musicsDecoder =
    D.at [ "tracks", "items" ] (D.list musicDecoder)


decode =
    let
        decodeHelp =
            D.decodeString musicsDecoder example
    in
    case decodeHelp of
        Ok m ->
            m

        Err _ ->
            []
