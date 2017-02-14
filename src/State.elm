module State exposing (init, update, subscriptions)

import Response exposing (..)
import Types exposing (..)
import Array exposing (..)
import WebSocket
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


websocketEndpoint : String
websocketEndpoint =
    "ws://game.clearercode.com:8000"


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
      , intersect1 = Nothing
      , intersect2 = Nothing
      , movedx = 0
      , movedy = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> Response Model Msg
update msg model =
    case msg of
        KeepAlive ->
            ( model, Cmd.none )

        Move x y ->
            let
                eqn1 =
                    { a = -10, b = -8, r = 15.275 }

                _ =
                    Debug.log "WebSocket" ((toString (eqn1.a)) ++ (toString (eqn1.b)))

                --solveEqn ({ a = -10, b = -8, r = 15.275 })
            in
                ( model, Cmd.none )

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
                                    if a.first.x == b.first.x then
                                        if a.first.y == b.first.y then
                                            { x = a.first.x, y = a.first.y }
                                        else
                                            { x = a.first.x, y = b.first.y }
                                    else if a.second.x == b.second.x then
                                        if a.second.y == b.second.y then
                                            { x = a.second.x, y = a.second.y }
                                        else
                                            { x = a.second.x, y = b.second.y }
                                    else if a.first.x == b.second.x then
                                        if a.first.y == b.second.y then
                                            { x = a.first.x, y = a.first.y }
                                        else
                                            { x = a.first.x, y = b.second.y }
                                    else if a.second.y == b.first.y then
                                        { x = a.second.x, y = a.second.y }
                                    else
                                        { x = a.second.x, y = b.first.y }

                solution =
                    { x = tempsolution.x + firstx, y = tempsolution.y + firsty }
            in
                ( { model | movedx = firstx, intersect1 = solved1, intersect2 = solved2, movedy = firsty, lastMessage = Just response, eqn1 = first, eqn2 = second, eqn3 = third }
                , Cmd.none
                )

        SendWebSocket ->
            let
                _ =
                    Debug.log "WebSocket" "1"
            in
                ( model, Cmd.none )


null_or_list_decoder : Json.Decoder (Maybe (List String))
null_or_list_decoder =
    Json.oneOf [ Json.null Nothing, Decode.map Just (Json.list Json.string) ]


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
