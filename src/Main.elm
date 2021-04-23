module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Maybe exposing (withDefault)
import Random
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, viewBox, width, x1, x2, y1, y2)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { pointsVirt : List PointVirt
    , canvasSize : Float
    , groundSpaceR : Float
    , avgDeg : Float
    , n : Int

    -- derived information
    , points : List Point
    , pointPairs : List ( Point, Point )
    }


maxN : Int
maxN =
    200


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pointsVirt = []
      , canvasSize = 1000
      , groundSpaceR = 8
      , avgDeg = 4
      , n = 100
      , points = []
      , pointPairs = []
      }
    , Random.generate NewPoints (randomPointsVirt maxN)
    )


sortedPairs : List Point -> List ( Point, Point )
sortedPairs points =
    let
        pairDist : ( Point, Point ) -> Float
        pairDist pair =
            dist (Tuple.first pair) (Tuple.second pair)

        filter : ( Point, Point ) -> Bool
        filter pair =
            (Tuple.first pair).phi < (Tuple.second pair).phi

        pairs : List ( Point, Point )
        pairs =
            List.filter filter
                (List.concatMap
                    (\p1 -> List.map (\p2 -> ( p1, p2 )) points)
                    points
                )
    in
    List.sortBy pairDist pairs



-- POINTS


type alias PointVirt =
    -- Point in polar coordinates with virtual radius rVirt.  Virtual
    -- radius means that rVirt ∈ [0, 1] represents a radius such that
    -- the relative area (w.r.t. the ground space) of a disk with
    -- radius rVirt is rVirt.
    { rVirt : Float
    , phi : Float
    }


type alias Point =
    -- Point in hyperbolic polar coordinates as well as canvas
    -- coordinates.  Additionally sinh(r) and cosh(r) are preprocessed
    -- for faster distance computation.
    { r : Float
    , phi : Float
    , sinhR : Float
    , coshR : Float
    , x : Float
    , y : Float
    }


randomPointVirt : Random.Generator PointVirt
randomPointVirt =
    Random.map2 PointVirt (Random.float 0 1) (Random.float 0 (2 * pi))


randomPointsVirt : Int -> Random.Generator (List PointVirt)
randomPointsVirt n =
    Random.list n randomPointVirt


toPoint : Float -> Float -> PointVirt -> Point
toPoint canvasSize groundSpaceR point =
    let
        r =
            acosh (point.rVirt * (cosh groundSpaceR - 1) + 1)

        offset =
            canvasSize / 2

        x =
            offset + offset * r / groundSpaceR * cos point.phi

        y =
            offset + offset * r / groundSpaceR * sin point.phi
    in
    { r = r
    , phi = point.phi
    , sinhR = sinh r
    , coshR = cosh r
    , x = x
    , y = y
    }



-- HYPERBOLIC MATH


sinh : Float -> Float
sinh x =
    (e ^ x - e ^ -x) / 2


cosh : Float -> Float
cosh x =
    (e ^ x + e ^ -x) / 2


acosh : Float -> Float
acosh x =
    logBase e (x + sqrt (x - 1) * sqrt (x + 1))


dist : Point -> Point -> Float
dist p1 p2 =
    let
        diff =
            abs (p1.phi - p2.phi)

        deltaPhi =
            min diff (2 * pi - diff)
    in
    acosh (p1.coshR * p2.coshR - p1.sinhR * p2.sinhR * cos deltaPhi)



-- UPDATE


type Msg
    = NewPoints (List PointVirt)
    | CanvasSizeChange String
    | GroundSpaceRChange String
    | AvgDegChange String
    | NChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPoints newPoints ->
            let
                pointsVirt =
                    newPoints

                points =
                    List.map (toPoint model.canvasSize model.groundSpaceR) (List.take model.n pointsVirt)
            in
            ( { model | pointsVirt = pointsVirt, pointPairs = sortedPairs points, points = points }
            , Cmd.none
            )

        CanvasSizeChange newValue ->
            ( { model
                | canvasSize = withDefault model.canvasSize (String.toFloat newValue)
              }
            , Cmd.none
            )

        GroundSpaceRChange input ->
            let
                inputF =
                    withDefault model.groundSpaceR (String.toFloat input)

                newR =
                    20 ^ inputF - 0.9999

                points =
                    List.map (toPoint model.canvasSize newR) (List.take model.n model.pointsVirt)
            in
            ( { model | groundSpaceR = newR, pointPairs = sortedPairs points, points = points }
            , Cmd.none
            )

        AvgDegChange input ->
            let
                avgDeg =
                    withDefault model.avgDeg (String.toFloat input)
            in
            ( { model
                | avgDeg = avgDeg
              }
            , Cmd.none
            )

        NChange input ->
            let
                n =
                    withDefault model.n (String.toInt input)
                        
                points =
                    List.map (toPoint model.canvasSize model.groundSpaceR) (List.take n model.pointsVirt)
            in
            ( { model | n = n, pointPairs = sortedPairs points, points = points }
            , Cmd.none-- Random.generate NewPoints (randomPointsVirt n)
            )



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
    let
        nrEdges =
            floor (toFloat model.n * model.avgDeg / 2)
    in
    div []
        [ div []
            [ slider 200 1200 1 model.canvasSize CanvasSizeChange
            , text "Canvas Size"
            ]
        , div []
            [ slider 10 200 10 (toFloat model.n) NChange
            , text ("n (" ++ String.fromInt model.n ++ ")")
            ]
        , div []
            [ slider 2 16 0.2 model.avgDeg AvgDegChange
            , text ("Avg Deg (" ++ String.fromFloat model.avgDeg ++ ")")
            ]
        , div []
            [ slider 0 1 0.01 (logBase 20 (model.groundSpaceR + 0.9999)) GroundSpaceRChange
            , text
                ("Ground Space Radius ("
                    ++ String.fromFloat model.groundSpaceR
                    ++ ")"
                )
            ]
        , div []
            [ canvas model.canvasSize
                (drawGroundSpace model.canvasSize
                    :: List.map drawPoint model.points
                    ++ List.map drawLine (List.take nrEdges model.pointPairs)
                )
            ]
        ]



-- DRAWING SUBROUTINES


canvas : Float -> List (Svg.Svg msg) -> Html msg
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


drawPoint : Point -> Svg.Svg msg
drawPoint point =
    Svg.circle
        [ cx (String.fromFloat point.x)
        , cy (String.fromFloat point.y)
        , r "3"
        ]
        []


drawLine : ( Point, Point ) -> Svg.Svg msg
drawLine points =
    let
        p1 =
            Tuple.first points

        p2 =
            Tuple.second points
    in
    Svg.line
        [ x1 (String.fromFloat p1.x)
        , y1 (String.fromFloat p1.y)
        , x2 (String.fromFloat p2.x)
        , y2 (String.fromFloat p2.y)
        , stroke "black"
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
