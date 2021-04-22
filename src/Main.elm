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
    -- Point in polar coordinates with virtual radius rVirt.  Virtual
    -- radius means that rVirt ∈ [0, 1] represents a radius such that
    -- the relative area (w.r.t. the ground space) of a disk with
    -- radius rVirt is rVirt.
    { rVirt : Float
    , phi : Float
    }


type alias PointR =
    -- Point in Cartesian coordinates in R² (coordinate system of the
    -- svg output)
    { x : Float
    , y : Float
    }


type alias PointH =
    -- Point in hyperbolic polar coordinates.
    { r : Float
    , phi : Float
    }


randomPoint : Random.Generator Point
randomPoint =
    Random.map2 Point (Random.float 0 1) (Random.float 0 (2 * pi))


randomPoints : Int -> Random.Generator (List Point)
randomPoints n =
    Random.list n randomPoint


toPointR : Float -> Float -> Point -> PointR
toPointR canvasSize groundSpaceR point =
    let
        offset : Float
        offset =
            canvasSize / 2
        
        p : PointH
        p = 
            toPointH groundSpaceR point
    in
    { x = offset + offset * p.r / groundSpaceR * cos p.phi
    , y = offset + offset * p.r / groundSpaceR * sin p.phi
    }


toPointH : Float -> Point -> PointH
toPointH groundSpaceR point =
    { r = acosh (point.rVirt * (cosh groundSpaceR - 1) + 1)
    , phi = point.phi
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
    | GroundSpaceRChange String


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

        GroundSpaceRChange newValue ->
            ( { model
                | groundSpaceR = withDefault model.groundSpaceR (String.toFloat newValue)
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


slider : Float -> Float -> Float -> Float -> (String -> Msg) -> Html Msg
slider min max step val msg =
    input
        [ type_ "range"
        , Html.Attributes.min (String.fromFloat min)
        , Html.Attributes.max (String.fromFloat max)
        , Html.Attributes.step (String.fromFloat step)
        , value (String.fromFloat val)
        , onInput msg
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Canvas Size"
            , slider 200 1200 1 model.canvasSize CanvasSizeChange
            ]
        , div []
            [ text "Ground Space Radius"
            , slider 0.2 25 0.2 model.groundSpaceR GroundSpaceRChange
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
            toPointR canvasSize groundSpaceR point
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
