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
    { points : List PointVirt
    , canvasSize : Float
    , groundSpaceR : Float
    , avgDeg : Float
    , n : Int

    -- derived information
    , pointsH : List PointH
    , pointPairs : List ( PointH, PointH )
    }


initialN : Int
initialN =
    100


init : () -> ( Model, Cmd Msg )
init _ =
    ( { points = []
      , canvasSize = 1000
      , groundSpaceR = 8
      , avgDeg = 4
      , n = initialN
      , pointsH = []
      , pointPairs = []
      }
    , Random.generate NewPoints (randomPoints initialN)
    )


sortedPairs : List PointH -> List ( PointH, PointH )
sortedPairs points =
    let
        pairDist : ( PointH, PointH ) -> Float
        pairDist pair =
            dist (Tuple.first pair) (Tuple.second pair)

        filter : ( PointH, PointH ) -> Bool
        filter pair =
            (Tuple.first pair).phi < (Tuple.second pair).phi

        pairs : List ( PointH, PointH )
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


type alias PointR =
    -- Point in Cartesian coordinates in R² (coordinate system of the
    -- svg output)
    { x : Float
    , y : Float
    }


type alias PointH =
    -- Point in hyperbolic polar coordinates with some info
    -- preprocessed.
    { r : Float
    , phi : Float
    , sinhR : Float
    , coshR : Float
    -- , x : Float
    -- , y : Float
    }


randomPoint : Random.Generator PointVirt
randomPoint =
    Random.map2 PointVirt (Random.float 0 1) (Random.float 0 (2 * pi))


randomPoints : Int -> Random.Generator (List PointVirt)
randomPoints n =
    Random.list n randomPoint


toPointR : Float -> Float -> PointH -> PointR
toPointR canvasSize groundSpaceR point =
    let
        offset : Float
        offset =
            canvasSize / 2
    in
    { x = offset + offset * point.r / groundSpaceR * cos point.phi
    , y = offset + offset * point.r / groundSpaceR * sin point.phi
    }


toPointH : Float -> PointVirt -> PointH
toPointH groundSpaceR point =
    let
        newR =
            acosh (point.rVirt * (cosh groundSpaceR - 1) + 1)
    in
    { r = newR
    , phi = point.phi
    , sinhR = sinh newR
    , coshR = cosh newR
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


dist : PointH -> PointH -> Float
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
                points =
                    newPoints

                pointsH =
                    List.map (toPointH model.groundSpaceR) points
            in
            ( { model | points = points, pointPairs = sortedPairs pointsH, pointsH = pointsH }
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

                pointsH =
                    List.map (toPointH newR) model.points
            in
            ( { model | groundSpaceR = newR, pointPairs = sortedPairs pointsH, pointsH = pointsH }
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
            in
            ( { model | n = n }
            , Random.generate NewPoints (randomPoints n)
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
        n =
            toFloat (List.length model.points)

        nrEdges =
            floor (n * model.avgDeg / 2)
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
                    :: List.map
                        (drawPoint model.canvasSize model.groundSpaceR)
                        model.pointsH
                    ++ List.map
                        (drawLine model.canvasSize model.groundSpaceR)
                        (List.take nrEdges model.pointPairs)
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


drawPoint : Float -> Float -> PointH -> Svg.Svg msg
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


drawLine : Float -> Float -> ( PointH, PointH ) -> Svg.Svg msg
drawLine canvasSize groundSpaceR points =
    let
        p1 =
            Tuple.first points

        p2 =
            Tuple.second points

        xy1 =
            toPointR canvasSize groundSpaceR p1

        xy2 =
            toPointR canvasSize groundSpaceR p2
    in
    Svg.line
        [ x1 (String.fromFloat xy1.x)
        , y1 (String.fromFloat xy1.y)
        , x2 (String.fromFloat xy2.x)
        , y2 (String.fromFloat xy2.y)
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
