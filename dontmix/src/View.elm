module View exposing (view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Msg exposing (Msg(..))
import Url



-- VIEW --------------------------------------------------


view : Model -> Browser.Document Msg
view m =
    { title = "Dontmix"
    , body =
        [ div []
            [ img logoAttributes []
            , div [ style "margin" "0 auto", style "width" "fit-content" ]
                [ input [ placeholder "Type here", onInput Save, style "width" "300px" ] []
                , button [ onClick <| Search, style "margin" "0 10px" ] [ text "Search Music" ]
                , button [ onClick <| RecommendMusics, style "margin-right" "10px" ] [ text "Recommend Musics" ]
                , button [ onClick <| SavePage ] [ text "Save Page" ]
                ]
            , div [ style "display" "flex", style "height" "30vw" ]
                [ showPlaylist m.searchedMusics
                , showSavedMusics "Selected Musics" m.musics
                , showSavedMusics "Recommended Musics" m.recommendedMusics
                ]
            ]
        ]
    }


showPlaylist : List Music -> Html Msg
showPlaylist playlist =
    let
        musics =
            List.map (showMusic True) playlist
    in
    div [ style "width" "100%", style "margin-top" "50px" ]
        [ h2 headerAttributes [ text "Search Results" ]
        , ul (musicListAttributes ++ [ style "list-style" "none", style "padding" "0" ]) musics
        ]


showSavedMusics : String -> List Music -> Html Msg
showSavedMusics header playlist =
    let
        musics =
            List.map (showMusic False) playlist
    in
    div savedMusicsAttributes
        [ h2 headerAttributes [ text header ]
        , ol musicListAttributes musics
        ]


showMusic : Bool -> Music -> Html Msg
showMusic clickable music =
    let
        attrs =
            if clickable then
                (onClick <| Add [ music ]) :: style "cursor" "pointer" :: liAttributes

            else
                liAttributes
    in
    li attrs
        [ p []
            [ b [ style "color" "#620029" ] [ text "Song: " ], text music.name ]
        , p []
            [ b [ style "color" "#620029" ] [ text "Artist: " ], text music.artist ]
        , p []
            [ b [ style "color" "#620029" ] [ text "Album: " ], text music.album ]
        , p []
            [ b [ style "color" "#620029" ] [ text "Duration (ms): " ], text (String.fromInt music.duration) ]
        ]



-- CSS --------------------------------------------------


liAttributes =
    [ style "padding" "5px", style "border-bottom" "1px solid grey" ]


musicListAttributes =
    [ style "max-height" "350px"
    , style "overflow-y" "scroll"
    , style "min-width" "300px"
    ]


logoAttributes =
    [ src "../assets/dontmix-inline.png"
    , style "display" "block"
    , style "width" "20%"
    , style "height" "auto"
    , style "margin" "40px auto"
    ]


headerAttributes =
    [ style "background" "-webkit-linear-gradient(#800080, #550000)"
    , style "-webkit-background-clip" "text"
    , style "font-size" "30px"
    , style "-webkit-text-fill-color" "transparent"
    , style "text-align" "center"
    ]


savedMusicsAttributes =
    [ style "width" "100%"
    , style "border-left" "1px solid #800080"
    , style "margin-top" "50px"
    ]
