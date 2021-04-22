module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (max, min, type_, value)
import Html.Events exposing (onInput)
import Maybe exposing (withDefault)
import Random
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, viewBox, width)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { points : List Point
    , canvasSize : Float
    , groundSpaceR : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { points = []
      , canvasSize = 1000
      , groundSpaceR = 10
      }
    , Random.generate NewPoints (randomPoints 50)
    )



-- MODEL: points


type alias Point =
    -- rVirt: virtual radius in [0, 1] such that the area of a disk
    -- with radius rVirt relative to the ground space is rVirt
    { rVirt : Float

    -- phi: angle in [0, 2 * pi]
    , phi : Float
    }


randomPoint : Random.Generator Point
randomPoint =
    Random.map2 Point (Random.float 0 1) (Random.float 0 (2 * pi))


randomPoints : Int -> Random.Generator (List Point)
randomPoints n =
    Random.list n randomPoint


type alias XyPoint =
    { x : Float
    , y : Float
    }


xyPoint : Float -> Float -> Point -> XyPoint
xyPoint canvasSize groundSpaceR point =
    let
        offset =
            canvasSize / 2

        r =
            acosh (point.rVirt * (cosh groundSpaceR - 1) + 1)
    in
    { x = offset + offset * r / groundSpaceR * cos point.phi
    , y = offset + offset * r / groundSpaceR * sin point.phi
    }



-- HYPERBOLIC MATH


cosh : Float -> Float
cosh x =
    (e ^ x + e ^ -x) / 2


acosh : Float -> Float
acosh x =
    logBase e (x + sqrt (x - 1) * sqrt (x + 1))



-- UPDATE


type Msg
    = NewPoints (List Point)
    | CanvasSizeChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPoints newPoints ->
            ( { model | points = newPoints }
            , Cmd.none
            )

        CanvasSizeChange newValue ->
            ( { model
                | canvasSize = withDefault model.canvasSize (String.toFloat newValue)
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "test"
            , input
                [ type_ "range"
                , Html.Attributes.min "200"
                , Html.Attributes.max "1200"
                , value (String.fromFloat model.canvasSize)
                , onInput CanvasSizeChange
                ]
                []
            ]
        , div []
            [ canvas model.canvasSize
                (drawGroundSpace model.canvasSize
                    :: List.map
                        (drawPoint model.canvasSize model.groundSpaceR)
                        model.points
                )
            ]
        ]



-- DRAWING SUBROUTINES


canvas canvasSize =
    let
        size =
            String.fromFloat canvasSize
    in
    svg
        [ width size
        , height size
        , viewBox ("0 0 " ++ size ++ " " ++ size)
        ]


drawPoint : Float -> Float -> Point -> Svg.Svg msg
drawPoint canvasSize groundSpaceR point =
    let
        xy =
            xyPoint canvasSize groundSpaceR point
    in
    Svg.circle
        [ cx (String.fromFloat xy.x)
        , cy (String.fromFloat xy.y)
        , r "3"
        ]
        []


drawGroundSpace : Float -> Svg.Svg msg
drawGroundSpace canvasSize =
    let
        offset =
            String.fromFloat (canvasSize / 2)
    in
    circle
        [ cx offset
        , cy offset
        , r offset
        , fill "none"
        , stroke "black"
        ]
        []
