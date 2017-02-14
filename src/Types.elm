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
    | SetName
    | MakeSend Bool
    | SetColour
    | Move Float Float
    | SendWebSocket


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


type alias Message =
    { tag : String
    , contents : String
    }


type alias Model =
    { lastMessage : Maybe String
    , movedx : Int
    , movedy : Int
    , x : Float
    , y : Float
    , send : Bool
    , eqn1 : Maybe Eqn
    , eqn2 : Maybe Eqn
    , eqn3 : Maybe Eqn
    , solution : { x : Int, y : Int }
    , intersect1 : Maybe DoubleIntersect
    , intersect2 : Maybe DoubleIntersect
    }
