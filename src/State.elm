module State exposing (init, update, subscriptions)

import Response exposing (..)
import Types exposing (..)
import Array exposing (..)
import WebSocket
import Time
import Bitwise
import Task
import Process
import Html.Attributes as H exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Json.Encode as Encode exposing (..)


websocketEndpoint : String
websocketEndpoint =
    "ws://game.clearercode.com:8000"


hyp : Int -> Int -> Float
hyp x y =
    toFloat ((x ^ 2) + (y ^ 2))
        |> sqrt


solveForx a b c =
    let
        _ =
            Debug.log "A" ("a:" ++ (toString a) ++ "b:" ++ (toString b) ++ "c:" ++ (toString c))

        first =
            ((negate b) + sqrt ((b * b) - (4 * (a * c)))) / (2 * a)

        second =
            ((negate b) - sqrt ((b * b) - (4 * (a * c)))) / (2 * a)
    in
        { first = first, second = second }


findIntersections : Float -> Eqn -> DoubleIntersect
findIntersections r a =
    let
        _ =
            Debug.log "B" ("a:" ++ (toString a.a) ++ "b:" ++ (toString a.b) ++ "r:" ++ (toString a.r) ++ "R:" ++ (toString r))

        x =
            --            solveForx (toFloat (4 * ((a.b ^ 2) - (a.a ^ 2)))) ((negate 4) * ((toFloat a.a) * ((toFloat ((a.a ^ 2) + (a.b ^ 2))) - (r ^ 2)))) ((((toFloat ((a.a ^ 2) + (a.b ^ 2))) - (a.r ^ 2)) ^ 2) - (r ^ 2))
            let
                middle =
                    toFloat ((a.a ^ 2) + (a.b ^ 2)) - (a.r ^ 2) + (r ^ 2)

                p =
                    toFloat (4 * ((a.b ^ 2) + (a.a ^ 2)))

                b =
                    (toFloat ((negate 4) * a.a)) * middle

                c =
                    (middle ^ 2) - ((r ^ 2) * (toFloat (4 * (a.b ^ 2))))

                _ =
                    Debug.log "P" ("P: " ++ (toString p) ++ " B: " ++ (toString b) ++ " C: " ++ (toString c))
            in
                solveForx p b c

        middle =
            toFloat ((a.a ^ 2) + (a.b ^ 2)) - (a.r ^ 2) + (r ^ 2)

        b =
            (toFloat ((negate 4) * a.a)) * middle

        y1 =
            ((negate (a.r ^ 2)) + (r ^ 2) + (toFloat (a.a ^ 2)) + (toFloat (a.b ^ 2)) - (toFloat (2 * a.a) * x.first)) / (toFloat (2 * a.b))

        y2 =
            ((negate (a.r ^ 2)) + (r ^ 2) + (toFloat (a.a ^ 2)) + (toFloat (a.b ^ 2)) - (toFloat (2 * a.a) * x.second)) / (toFloat (2 * a.b))
    in
        { first = { x = round x.first, y = round y1 }, second = { x = round x.second, y = round y2 } }


solveEqn : Eqn -> Eqn -> DoubleIntersect
solveEqn eqn1 eqn2 =
    let
        _ =
            Debug.log "Eqn" ((toString (eqn1.a)) ++ (toString (eqn1.b)))
    in
        { first = { x = 1, y = 2 }
        , second = { x = 1, y = 2 }
        }


circleToEqn : Eqn -> (Float -> Float)
circleToEqn eqn x =
    sqrt
        ((eqn.r * eqn.r)
            - (((x - (toFloat eqn.a)) * (x - (toFloat (eqn.a)))))
        )
        + (toFloat eqn.b)


init : Response Model Msg
init =
    ( { lastMessage = Nothing
      , x = 0
      , y = 0
      , eqn1 = Nothing
      , eqn2 = Nothing
      , eqn3 = Nothing
      , receiving = True
      , remainingToPoint = 0
      , solution = { x = 0, y = 0 }
      , intersect1 = Nothing
      , intersect2 = Nothing
      , movedx = 0
      , send = False
      , movedy = 0
      }
    , Cmd.none
    )


move x y =
    Move x y


update : Msg -> Model -> Response Model Msg
update msg model =
    -- let
    --     _ =
    --         Debug.log "update" msg
    -- in
    case msg of
        MakeSend t ->
            ( { model | send = not t }, Cmd.none )

        SendWebSocket ->
            -- let
            --     _ =
            --         Debug.log "keep alive " "alive"
            -- in
            ( model, Cmd.none )

        KeepAlive ->
            -- let
            --     _ =
            --         Debug.log "keep alive " "alive"
            -- in
            ( model, Cmd.none )

        ReturnToHome ->
            let
                _ =
                    Debug.log "Sending Move" "1"

                hypot =
                    hyp model.solution.x model.solution.y

                remaining =
                    if model.remainingToPoint - 1 < 0 then
                        0
                    else
                        model.remainingToPoint - 1

                receiving =
                    case remaining of
                        0 ->
                            True

                        _ ->
                            False

                cmd =
                    case remaining of
                        0 ->
                            [ WebSocket.send websocketEndpoint payload_string2
                            ]

                        _ ->
                            [ WebSocket.send websocketEndpoint payload_string2
                            , Task.perform (\_ -> ReturnToHome) (Process.sleep (0.5 * Time.second))
                            ]

                payload2 =
                    object
                        [ ( "tag", Encode.string "Move" )
                        , ( "contents", Encode.object [ ( "x", Encode.int (negate model.solution.x) ), ( "y", Encode.int (negate model.solution.y) ) ] )
                        ]

                payload_string2 =
                    encode 0 payload2

                --    "{\"tag\": \"Move\", \"contents\": \"{x: -1, y: 0}\"}"
                --    "{\"tag\": \"SetName\", \"contents\": \"Team One\"}"
                _ =
                    Debug.log "payload1" payload_string2
            in
                ( { model | receiving = receiving, remainingToPoint = remaining, x = (model.x), y = model.y }
                , Cmd.batch
                    cmd
                )

        MoveToSolution ->
            let
                _ =
                    Debug.log "Sending Move" "1"

                hypot =
                    hyp model.solution.x model.solution.y

                remaining =
                    if model.remainingToPoint - 1 < 1 then
                        round hypot
                    else
                        model.remainingToPoint - 1

                cmd =
                    case model.remainingToPoint - 1 of
                        0 ->
                            [ WebSocket.send websocketEndpoint payload_string2
                            , Task.perform (\_ -> ReturnToHome) (Process.sleep (0.5 * Time.second))
                            ]

                        _ ->
                            [ WebSocket.send websocketEndpoint payload_string2
                            , Task.perform (\_ -> MoveToSolution) (Process.sleep (0.5 * Time.second))
                            ]

                payload2 =
                    object
                        [ ( "tag", Encode.string "Move" )
                        , ( "contents", Encode.object [ ( "x", Encode.int model.solution.x ), ( "y", Encode.int model.solution.y ) ] )
                        ]

                payload_string2 =
                    encode 0 payload2

                --    "{\"tag\": \"Move\", \"contents\": \"{x: -1, y: 0}\"}"
                --    "{\"tag\": \"SetName\", \"contents\": \"Team One\"}"
                _ =
                    Debug.log "payload1" payload_string2
            in
                ( { model | receiving = False, remainingToPoint = remaining, x = (model.x), y = model.y }
                , Cmd.batch
                    cmd
                )

        Move x y ->
            let
                _ =
                    Debug.log "Sending Move" "1"

                payload2 =
                    object
                        [ ( "tag", Encode.string "Move" )
                        , ( "contents", Encode.object [ ( "x", Encode.float x ), ( "y", Encode.float y ) ] )
                        ]

                payload_string2 =
                    encode 0 payload2

                --    "{\"tag\": \"Move\", \"contents\": \"{x: -1, y: 0}\"}"
                --    "{\"tag\": \"SetName\", \"contents\": \"Team One\"}"
                _ =
                    Debug.log "payload1" payload_string2
            in
                ( { model | x = model.x + x, y = model.y + y }
                , Cmd.batch
                    [ WebSocket.send websocketEndpoint payload_string2
                    ]
                )

        Receive response ->
            let
                decoded =
                    (Json.decodeString decode response)

                array =
                    case decoded of
                        Ok a ->
                            Array.fromList a.gpss

                        Err a ->
                            Array.empty

                firstInitial =
                    case (get 0 array) of
                        Nothing ->
                            Nothing

                        Just a ->
                            Just (gpssToEqn a)

                firsty =
                    case firstInitial of
                        Nothing ->
                            0

                        Just a ->
                            a.b

                firstx =
                    case firstInitial of
                        Nothing ->
                            0

                        Just a ->
                            a.a

                first =
                    case firstInitial of
                        Nothing ->
                            Nothing

                        Just a ->
                            Just { r = a.r, a = 0, b = 0 }

                secondInitial =
                    case (get 1 array) of
                        Nothing ->
                            Nothing

                        Just a ->
                            Just (gpssToEqn a)

                thirdInitial =
                    case (get 2 array) of
                        Nothing ->
                            Nothing

                        Just a ->
                            Just (gpssToEqn a)

                second =
                    case secondInitial of
                        Nothing ->
                            Nothing

                        Just a ->
                            Just { r = a.r, a = a.a - firstx, b = a.b - firsty }

                third =
                    case thirdInitial of
                        Nothing ->
                            Nothing

                        Just a ->
                            Just { r = a.r, a = a.a - firstx, b = a.b - firsty }

                solved1 =
                    case first of
                        Nothing ->
                            Nothing

                        Just a ->
                            case second of
                                Nothing ->
                                    Nothing

                                Just b ->
                                    Just (findIntersections a.r b)

                solved2 =
                    case first of
                        Nothing ->
                            Nothing

                        Just a ->
                            case third of
                                Nothing ->
                                    Nothing

                                Just b ->
                                    Just (findIntersections a.r b)

                tempsolution =
                    case solved1 of
                        Nothing ->
                            { x = 0, y = 0 }

                        Just a ->
                            case solved2 of
                                Nothing ->
                                    { x = 0, y = 0 }

                                Just b ->
                                    if (a.first.x == b.first.x) && (a.first.y == b.first.y) then
                                        { x = a.first.x, y = a.first.y }
                                    else if (a.second.x == b.second.x) && (a.second.y == b.second.y) then
                                        { x = a.second.x, y = a.second.y }
                                    else if (a.first.x == b.second.x) && (a.first.y == b.second.y) then
                                        { x = a.first.x, y = a.first.y }
                                    else
                                        { x = a.second.x, y = b.first.y }

                solution =
                    { x = tempsolution.x + firstx, y = tempsolution.y + firsty }

                _ =
                    Debug.log "Sending Move" "1"

                payload2 =
                    object
                        [ ( "tag", Encode.string "Move" )
                        , ( "contents", Encode.object [ ( "x", Encode.float (toFloat solution.x) ), ( "y", Encode.float (toFloat solution.y) ) ] )
                        ]

                payload_string2 =
                    encode 0 payload2

                --    "{\"tag\": \"Move\", \"contents\": \"{x: -1, y: 0}\"}"
                --    "{\"tag\": \"SetName\", \"contents\": \"Team One\"}"
                _ =
                    Debug.log "payload1" payload_string2
            in
                case model.receiving of
                    True ->
                        ( { model | remainingToPoint = (round (hyp solution.x solution.y)), solution = solution, movedx = firstx, intersect1 = solved1, x = (toFloat solution.x), y = (toFloat solution.y), intersect2 = solved2, movedy = firsty, lastMessage = Just response, eqn1 = first, eqn2 = second, eqn3 = third }
                        , Cmd.batch
                            [ Task.perform (\_ -> MoveToSolution) (Process.sleep (0.1 * Time.second))
                            ]
                        )

                    False ->
                        ( model
                        , Cmd.none
                        )

        SetName ->
            let
                _ =
                    Debug.log "Sending WebSocket" "1"

                payload1 =
                    object
                        [ ( "tag", Encode.string "SetName" )
                        , ( "contents", Encode.string "Team 42" )
                        ]

                payload_string1 =
                    encode 0 payload1

                _ =
                    Debug.log "payload1" payload_string1
            in
                ( model
                , Cmd.batch
                    [ WebSocket.send websocketEndpoint payload_string1
                    , Task.perform (\_ -> Move 0.0 0.0) (Process.sleep (2 * Time.second))
                    ]
                )

        SetColour ->
            let
                _ =
                    Debug.log "Sending colour" "1"

                payload2 =
                    object
                        [ ( "tag", Encode.string "SetColor" )
                        , ( "contents", Encode.string "#ff0000" )
                        ]

                payload_string2 =
                    encode 0 payload2

                --    "{\"tag\": \"Move\", \"contents\": \"{x: -1, y: 0}\"}"
                --    "{\"tag\": \"SetName\", \"contents\": \"Team One\"}"
                _ =
                    Debug.log "payload1" payload_string2
            in
                ( model
                , Cmd.batch
                    [ WebSocket.send websocketEndpoint payload_string2
                    , Task.perform (\_ -> Move 0.0 0.0) (Process.sleep (2 * Time.second))
                    ]
                )



--    ( model, Cmd.none )
--


decode : Json.Decoder ServerResponse
decode =
    Json.map ServerResponse
        (Json.field "gpss" (Json.list gpssDecoder))


gpssToEqn : GPSS -> Eqn
gpssToEqn gpss =
    { r = gpss.distance
    , a = gpss.position.x
    , b = gpss.position.y
    }


gpssDecoder =
    Json.map2 GPSS
        ((Json.field "distance" Json.float))
        (Json.field "position" posDecoder)


posDecoder =
    Json.map2 Point
        ((Json.field "x" Json.int))
        (Json.field "y" Json.int)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen websocketEndpoint Receive
        , WebSocket.keepAlive websocketEndpoint
            |> Sub.map (always KeepAlive)
        ]
