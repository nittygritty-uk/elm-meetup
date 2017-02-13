module Types exposing (..)


type Msg
    = Receive String
    | KeepAlive
    | SendWebSocket


type alias Model =
    { lastMessage : Maybe String }
