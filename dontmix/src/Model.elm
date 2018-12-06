module Model exposing (Model, Music)

import Browser.Navigation as Nav
import Http
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
