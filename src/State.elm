module State exposing (init, update, subscriptions)

import Response exposing (..)
import Types exposing (..)
import WebSocket
import Json.Encode exposing (..)


websocketEndpoint : String
websocketEndpoint =
    "ws://game.clearercode.com:8000"


init : Response Model Msg
init =
    ( { lastMessage = Nothing, x = 0, y = 0 }
    , Cmd.none
    )


update : Msg -> Model -> Response Model Msg
update msg model =
    -- let
    --     _ =
    --         Debug.log "update" msg
    -- in
    case msg of
        KeepAlive ->
            -- let
            --     _ =
            --         Debug.log "keep alive " "alive"
            -- in
            ( model, Cmd.none )

        Move x y ->
            ( model, Cmd.none )

        Receive response ->
            ( { model | lastMessage = Just response }
            , Cmd.none
            )

        SendWebSocket ->
            let
                _ =
                    Debug.log "Sending WebSocket" "1"

                payload1 =
                    object
                        [ ( "tag", string "SetName" )
                        , ( "contents", string "Team 42" )
                        ]

                payload2 =
                    object
                        [ ( "tag", string "SetColor" )
                        , ( "contents", string "#ff0000" )
                        ]

                --[ ( "tag", Json.Encode.string "SetName" ), ( "contents", Json.Encode.string "Team Two" ) ]
                --    { tag = "SetName", contents = "Team Two" }
                payload_string1 =
                    encode 0 payload1

                payload_string2 =
                    encode 0 payload2

                --    "{\"tag\": \"Move\", \"contents\": \"{x: -1, y: 0}\"}"
                --    "{\"tag\": \"SetName\", \"contents\": \"Team One\"}"
                _ =
                    Debug.log "payload1" payload_string1
            in
                ( model
                , Cmd.batch
                    [ WebSocket.send websocketEndpoint payload_string1
                    , WebSocket.send websocketEndpoint payload_string2
                    ]
                )



--    ( model, Cmd.none )
--


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
