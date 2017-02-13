module View exposing (root)

import CDN exposing (bootstrap)
import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Html.Events
import Html.Events exposing (onClick)


root : Model -> Html Msg
root model =
    div
        [ style
            [ ( "padding", "15px" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            ]
        ]
        [ bootstrap.css
        , div []
            [ code [] [ text <| toString model ]
            , div []
                [ button [ onClick SendWebSocket ] [ text "Send a test message" ] ]
            ]
            , button [ onClick (Move 0 -1) ] [ text "Left" ]
            , button [ onClick (Move 1 0) ] [ text "Right" ]
            , button [ onClick (Move 0 1) ] [ text "Up" ]
            , button [ onClick (Move 0 -1) ] [ text "Down" ]
        ]
