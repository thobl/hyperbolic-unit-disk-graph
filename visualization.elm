module Main exposing (main)

import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Point =
    { r : Float
    , phi : Float
    }


type alias XYPoint =
    { x : Float
    , y : Float
    }


type alias Model =
    { points : List Point
    , diskRadius : Float
    }


init : Model
init =
    { points = [ { r = 0.2, phi = 0.4 }, { r = 0.4, phi = 0.7 }, { r = 0.7, phi = 0.5 } ]
    , diskRadius = 400
    }


xyPoint : Float -> Point -> XYPoint
xyPoint diskRadius point =
    { x = diskRadius + diskRadius * point.r * cos point.phi
    , y = diskRadius + diskRadius * point.r * sin point.phi
    }



-- UPDATE


update model =
    model



-- VIEW

svgPoint : Float -> Point -> Svg msg
svgPoint diskRadius point =
    let
        xy =
            xyPoint diskRadius point
    in
    circle
        [ cx (String.fromFloat xy.x)
        , cy (String.fromFloat xy.y)
        , r "3"
        ]
        []


view : Model -> Html msg
view model =
    let
        radString =
            String.fromFloat model.diskRadius

        diamString =
            String.fromFloat (model.diskRadius * 2)
    in
    svg
        [ width diamString
        , height diamString
        , viewBox ("0 0 " ++ diamString ++ " " ++ diamString)
        ]
        (List.map
            (svgPoint model.diskRadius)
            model.points
            ++ [ circle
                    [ cx radString
                    , cy radString
                    , r radString
                    , fill "none"
                    , stroke "black"
                    ]
                    []
               ]
        )
