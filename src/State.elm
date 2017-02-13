module State exposing (init, update, subscriptions)

import Response exposing (..)
import Types exposing (..)
import WebSocket


websocketEndpoint : String
websocketEndpoint =
    "ws://game.clearercode.com"


init : Response Model Msg
init =
    ( { lastMessage = Nothing }
    , Cmd.none
    )


update : Msg -> Model -> Response Model Msg
update msg model =
    case msg of
        KeepAlive ->
            ( model, Cmd.none )

        Move ->
            ( model, Cmd.none )

        Receive response ->
            ( { lastMessage = Just response }
            , Cmd.none
            )

        SendWebSocket ->
            let
                _ =
                    Debug.log "WebSocket" "1"
            in
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen websocketEndpoint Receive
        , WebSocket.keepAlive websocketEndpoint
            |> Sub.map (always KeepAlive)
        ]


down : Int -> Msg
down count =
    Move (negate count) 0


right : Int -> Msg
right count =
    Move count 0


up : Int -> Msg
up count =
    Move 0 count


left : Int -> Msg
left count =
    Move 0 (negate count)
