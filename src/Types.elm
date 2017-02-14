module Types exposing (..)


type alias ServerResponse =
    { gpss : List GPSS
    }


type alias GPSS =
    { distance : Float
    , position : Point
    }


type Msg
    = Receive String
    | KeepAlive
    | SendWebSocket
    | Move Int Int


type alias Eqn =
    { a : Int
    , b : Int
    , r : Float
    }


type alias Point =
    { x : Int, y : Int }


type alias DoubleIntersect =
    { first : Point
    , second : Point
    }


type alias Model =
    { lastMessage : Maybe String
    , movedx : Int
    , movedy : Int
    , x : Int
    , y : Int
    , eqn1 : Maybe Eqn
    , eqn2 : Maybe Eqn
    , eqn3 : Maybe Eqn
    , intersect1 : Maybe DoubleIntersect
    , intersect2 : Maybe DoubleIntersect
    }
