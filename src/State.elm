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
            let
                _ =
                    Debug.log "keep alive " "alive"
            in
                ( model, Cmd.none )

        Receive response ->
            let
                _ =
                    Debug.log "web server received " response
            in
                ( { lastMessage = Just response }
                , Cmd.none
                )

        SendWebSocket ->
            let
                _ =
                    Debug.log "WebSocket" "1"
            in
                ( model, WebSocket.send websocketEndpoint "{}" )



--    ( model, Cmd.none )
--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen websocketEndpoint Receive
        , WebSocket.keepAlive websocketEndpoint
            |> Sub.map (always KeepAlive)
        ]
