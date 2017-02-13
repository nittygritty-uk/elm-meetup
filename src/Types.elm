module Types exposing (..)


type Msg
    = Receive String
    | KeepAlive
    | SendWebSocket
    | Move Int Int


type alias Message =
    { tag : String
    , contents : String
    }


type alias Model =
    { lastMessage : Maybe String, x : Int, y : Int }
