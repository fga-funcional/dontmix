module Model exposing (Model, Music, init)

-- MODEL --------------------------------------------------


type alias Music =
    { name : String
    , artist : String
    , album : String
    , duration : Int
    }


type alias Model =
    { musics : List Music
    , input : String
    , searchedMusics : List Music
    , recommendedMusics : List Music
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { musics = []
      , input = ""
      , searchedMusics = []
      , recommendedMusics = []
      }
    , Cmd.none
    )
