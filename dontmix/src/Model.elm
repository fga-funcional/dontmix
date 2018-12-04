module Model exposing (Model, Music, init)

import Browser.Navigation as Nav
import Url



-- MODEL --------------------------------------------------


type alias Music =
    { id : String
    , name : String
    , artist : String
    , album : String
    , duration : Int
    }


type alias Model =
    { musics : List Music
    , input : String
    , searchedMusics : List Music
    , recommendedMusics : List Music
    , url : Url.Url
    , key : Nav.Key
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd msg )
init _ url key =
    ( { musics = []
      , input = ""
      , searchedMusics = []
      , recommendedMusics = []
      , url = url
      , key = key
      }
    , Cmd.none
    )
