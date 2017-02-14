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
            [ div []
                [ button [ onClick SendWebSocket ] [ text "Send a test message" ] ]
            , div []
                [ button [ onClick SetName ] [ text "Set name" ]
                , button [ onClick SetColour ] [ text "Set colour" ]
                , button [ onClick (MakeSend model.send) ] [ text "Make Send" ]
                ]
            , button [ onClick (Move -1.0 0.0) ] [ text "Left" ]
            , button [ onClick (Move 1.0 0.0) ] [ text "Right" ]
            , button [ onClick (Move (toFloat model.solution.x) (toFloat model.solution.y)) ] [ text "Move To Solution" ]
            , button [ onClick (Move (negate model.x) (negate model.y)) ] [ text "Return to Zero" ]
            , button [ onClick (Move 0.0 1.0) ] [ text "Up" ]
            , button [ onClick (Move 0.0 -1.0) ] [ text "Down" ]
            ]
        ]
