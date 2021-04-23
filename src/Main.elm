module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border
import Element.Input exposing (defaultThumb, labelLeft, labelRight, slider)
import Html exposing (Html)
import Maybe exposing (withDefault)
import Random
import Svg exposing (circle, line)
import Svg.Attributes exposing (cx, cy, r, stroke, x1, x2, y1, y2)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = viewNew
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
    250


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
    , Random.generate GeneratedPoints (randomPointsVirt maxN)
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
    = GeneratedPoints (List PointVirt)
    | InputNrVertices Float
    | InputCanvasSize Float
    | InputAvgDeg Float
    | InputGroundSpaceR Float


updatePoints : Model -> Model
updatePoints model =
    let
        points =
            List.map (toPoint model.canvasSize model.groundSpaceR)
                (List.take model.n model.pointsVirt)
    in
    { model | points = points, pointPairs = sortedPairs points }


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedPoints newPoints ->
            { model | pointsVirt = newPoints } |> updatePoints |> noCmd

        InputNrVertices n ->
            { model | n = round n } |> updatePoints |> noCmd

        InputCanvasSize size ->
            { model | canvasSize = size } |> updatePoints |> noCmd

        InputAvgDeg avgDeg ->
            { model | avgDeg = avgDeg } |> noCmd

        InputGroundSpaceR x ->
            { model | groundSpaceR = 20 ^ x - 0.9999 } |> updatePoints |> noCmd



-- VIEW


formatFloat x =
    String.fromFloat (toFloat (round (10000 * x)) / 10000)


sliderInt label value valueFun msg min max step =
    mySlider label (toFloat (valueFun value)) (String.fromInt value) msg (toFloat min) (toFloat max) (Just step)


sliderFloat label value valueFun msg min max =
    mySlider label (valueFun value) (formatFloat value) msg min max Nothing


mySlider label value stringValue msg min max step =
    row [ spacing 10, width (px 500) ]
        [ el [ alignLeft ] (text label)
        , el [ alignRight ]
            (slider
                [ width (px 200)
                , behindContent
                    (el [ width fill, height (px 2), centerY, Background.color (rgb 0.5 0.5 0.5) ] none)
                ]
                { onChange = msg
                , label = labelLeft [ centerY ] (text stringValue)
                , min = min
                , max = max
                , step = step
                , value = value
                , thumb = defaultThumb
                }
            )
        ]


viewNew : Model -> Html Msg
viewNew model =
    let
        nrEdges =
            floor (toFloat model.n * model.avgDeg / 2)
    in
    layout []
        (row [ padding 10, spacing 20 ]
            [ column [ alignTop, spacing 10 ]
                [ sliderInt "canvas size" (round model.canvasSize) identity InputCanvasSize 200 1200 1
                , sliderInt "number of vertices" model.n identity InputNrVertices 10 maxN 1
                , sliderFloat "average degree" model.avgDeg identity InputAvgDeg 2 16
                , sliderFloat "ground space radius" model.groundSpaceR (\x -> logBase 20 (x + 0.9999)) InputGroundSpaceR 0 1
                ]
            , el []
                (html
                    (canvas model.canvasSize
                        (drawGroundSpace model.canvasSize
                            :: List.map drawPoint model.points
                            ++ List.map drawLine (List.take nrEdges model.pointPairs)
                        )
                    )
                )
            ]
        )



-- DRAWING SUBROUTINES


canvas : Float -> List (Svg.Svg msg) -> Html msg
canvas canvasSize =
    let
        size =
            String.fromFloat canvasSize
    in
    Svg.svg
        [ Svg.Attributes.width size
        , Svg.Attributes.height size
        , Svg.Attributes.viewBox ("0 0 " ++ size ++ " " ++ size)
        ]


drawPoint : Point -> Svg.Svg msg
drawPoint point =
    circle
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
    line
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
        , Svg.Attributes.fill "none"
        , stroke "black"
        ]
        []
