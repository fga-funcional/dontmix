module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Msg exposing (Msg(..))



-- VIEW --------------------------------------------------


view : Model -> Html Msg
view m =
    div []
        [ h1 [] [ text "Dontmix" ]
        , input [ placeholder "Type here", onInput Save ] []
        , button [ onClick <| Search ] [ text "Search Music" ]
        , div [ style "display" "flex" ]
            [ showPlaylist m.searchedMusics
            , showSavedMusics m.musics
            ]
        ]


showPlaylist : List Music -> Html Msg
showPlaylist playlist =
    let
        musics =
            List.map (showMusic True) playlist
    in
    div []
        [ h2 [] [ text "Search Results" ]
        , ul (musicListAttributes ++ [ style "list-style" "none", style "padding" "0" ]) musics
        ]


showSavedMusics : List Music -> Html Msg
showSavedMusics playlist =
    let
        musics =
            List.map (showMusic False) playlist
    in
    div []
        [ h2 [] [ text "My Playlist" ]
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
            [ b [] [ text "Song: " ], text music.name ]
        , p []
            [ b [] [ text "Artist: " ], text music.artist ]
        , p []
            [ b [] [ text "Album: " ], text music.album ]
        , p []
            [ b [] [ text "Duration (ms): " ], text (String.fromInt music.duration) ]
        ]



-- CSS --------------------------------------------------


liAttributes =
    [ style "padding" "5px", style "border-bottom" "1px solid grey" ]


musicListAttributes =
    [ style "max-height" "450px"
    , style "overflow-y" "scroll"
    , style "min-width" "300px"
    ]
