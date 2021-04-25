module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (defaultThumb, labelLeft, slider)
import Html exposing (Html)
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
    , canvasSize : Int
    , groundSpaceR : Float
    , avgDeg : Float
    , n : Int

    -- derived information
    , points : List Point
    , pointPairs : List ( Point, Point )
    , thresholdRadius : Float
    }


maxN : Int
maxN =
    400


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pointsVirt = []
      , canvasSize = 1000
      , groundSpaceR = 8.5
      , avgDeg = 6
      , n = 200
      , points = []
      , pointPairs = []
      , thresholdRadius = 1
      }
    , Random.generate GeneratedPoints (randomPointsVirt maxN)
    )


pairs : List a -> List ( a, a )
pairs elements =
    List.indexedMap
        (\index elt1 ->
            List.map
                (\elt2 -> Tuple.pair elt1 elt2)
                (List.drop (index + 1) elements)
        )
        elements
        |> List.concat



-- POINTS


type alias PointVirt =
    -- Point in polar coordinates with virtual radius rVirt.  Virtual
    -- radius means that rVirt ∈ [0, 1] represents a radius such that
    -- the relative area (w.r.t. the ground space) of a disk with
    -- radius rVirt is rVirt.
    { rVirt : Float, phi : Float }


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


toPoint : Int -> Float -> PointVirt -> Point
toPoint canvasSize groundSpaceR point =
    let
        r =
            acosh (point.rVirt * (cosh groundSpaceR - 1) + 1)

        offset =
            toFloat canvasSize / 2
    in
    { r = r
    , phi = point.phi
    , sinhR = sinh r
    , coshR = cosh r
    , x = offset + offset * r / groundSpaceR * cos point.phi
    , y = offset + offset * r / groundSpaceR * sin point.phi
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


pairDist : ( Point, Point ) -> Float
pairDist pair =
    dist (Tuple.first pair) (Tuple.second pair)



-- UPDATE


updatePoints : Model -> Model
updatePoints model =
    { model
        | points =
            List.map
                (toPoint model.canvasSize model.groundSpaceR)
                (List.take model.n model.pointsVirt)
    }
        |> updatePointPairs


updatePointPairs : Model -> Model
updatePointPairs model =
    { model | pointPairs = pairs model.points } |> updatePointPairOrder


updatePointPairOrder : Model -> Model
updatePointPairOrder model =
    { model | pointPairs = List.sortBy pairDist model.pointPairs } |> updateThresholdR


updateThresholdR : Model -> Model
updateThresholdR model =
    let
        nrEdges =
            floor (toFloat model.n * model.avgDeg / 2)

        lastPair =
            List.drop (nrEdges - 1) model.pointPairs |> List.head

        thresholdR =
            case lastPair of
                Just pair ->
                    pairDist pair

                Nothing ->
                    2 * model.groundSpaceR
    in
    { model | thresholdRadius = thresholdR }


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


type Msg
    = GeneratedPoints (List PointVirt)
    | InputCanvasSize Float
    | InputNrVertices Float
    | InputAvgDeg Float
    | InputGroundSpaceR Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedPoints newPoints ->
            { model | pointsVirt = newPoints } |> updatePoints |> noCmd

        InputNrVertices n ->
            { model | n = max (round n) (ceiling model.avgDeg + 1) } |> updatePoints |> noCmd

        InputCanvasSize size ->
            { model | canvasSize = round size } |> updatePoints |> noCmd

        InputAvgDeg avgDeg ->
            { model | avgDeg = min avgDeg (toFloat model.n - 1) } |> updateThresholdR |> noCmd

        InputGroundSpaceR x ->
            { model | groundSpaceR = inputToGroundSpaceR x } |> updatePoints |> noCmd



-- VIEW


inputToGroundSpaceR : Float -> Float
inputToGroundSpaceR input =
    20.999 ^ input - 0.999


groundSpaceRToInput : Float -> Float
groundSpaceRToInput r =
    logBase 20.999 (r + 0.999)


formatFloat : Float -> String
formatFloat x =
    String.fromFloat (toFloat (round (1000 * x)) / 1000)


type alias SliderSettings =
    { label : String
    , onChange : Float -> Msg
    , value : Float
    , valueFun : Float -> Float
    , min : Float
    , max : Float
    }


mySlider : SliderSettings -> Element Msg
mySlider s =
    row [ spacing 10, width (px 600) ]
        [ el [ alignLeft ] (text s.label)
        , el [ alignRight ]
            (slider
                -- slider optics
                [ width (px 350)
                , behindContent
                    (el
                        [ width fill
                        , height (px 2)
                        , centerY
                        , Background.color (rgb 0.5 0.5 0.5)
                        ]
                        none
                    )
                ]
                -- slider functionality
                { onChange = s.onChange
                , label = labelLeft [ centerY ] (text (formatFloat s.value))
                , min = s.min
                , max = s.max
                , step = Nothing
                , value = s.valueFun s.value
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
    layout
        [ Font.size 18 ]
        (row [ padding 10, spacing 40 ]
            [ column [ alignTop, spacing 10 ]
                [ title1 "Hyperbolic Unit Disk Graph"
                , mySlider
                    { label = "canvas size"
                    , onChange = InputCanvasSize
                    , value = toFloat model.canvasSize
                    , valueFun = identity
                    , min = 200
                    , max = 1200
                    }
                , mySlider
                    { label = "number of vertices"
                    , onChange = InputNrVertices
                    , value = toFloat model.n
                    , valueFun = identity
                    , min = 10
                    , max = toFloat maxN
                    }
                , mySlider
                    { label = "average degree"
                    , onChange = InputAvgDeg
                    , value = model.avgDeg
                    , valueFun = identity
                    , min = 2
                    , max = 16
                    }
                , mySlider
                    { label = "ground space radius"
                    , onChange = InputGroundSpaceR
                    , value = model.groundSpaceR
                    , valueFun = groundSpaceRToInput
                    , min = 0
                    , max = 1
                    }
                , description model
                ]
            , el [ alignTop, paddingXY 0 20 ]
                (html
                    (canvas model.canvasSize
                        (drawGroundSpace model
                            :: drawThresholdRadius model
                            :: List.map drawPoint model.points
                            ++ List.map drawLine (List.take nrEdges model.pointPairs)
                        )
                    )
                )
            ]
        )


title1 : String -> Element Msg
title1 t =
    el [ paddingXY 0 10, Font.size 26, Font.bold ] (text t)


title2 : String -> Element Msg
title2 t =
    el [ paddingXY 0 10, Font.size 22, Font.bold ] (text t)


wildcardText : Model -> String -> Element Msg
wildcardText model s =
    text
        (String.concat
            (List.map
                (\part ->
                    case part of
                        "n" ->
                            String.fromInt model.n

                        "avgDeg" ->
                            formatFloat model.avgDeg

                        "groundSpaceR" ->
                            formatFloat model.groundSpaceR

                        "thresholdRadius" ->
                            formatFloat model.thresholdRadius

                        "thresholdRadiusRel" ->
                            formatFloat (model.thresholdRadius / model.groundSpaceR)

                        "m" ->
                            String.fromInt (floor (toFloat model.n * model.avgDeg / 2))

                        _ ->
                            part
                )
                (String.split "$" s)
            )
        )


description : Model -> Element Msg
description model =
    let
        hyperbolic =
            model.thresholdRadius / model.groundSpaceR > 0.6

        euclidean =
            model.thresholdRadius / model.groundSpaceR < 0.25
    in
    textColumn [ paddingXY 0 15, spacing 10 ]
        [ title2 "What do I see?"
        , paragraph []
            [ wildcardText model """

You see a disk of radius $groundSpaceR$ in the hyperbolic plane
represented by the black circle.  This is the ground space.  Within the
ground space, you see a graph with $n$ vertices and $m$ edges, where
two vertices are connected by an edge if their hyperbolic distance
does not exceed the threshold of $thresholdRadius$
($thresholdRadiusRel$ times the radius of the ground space).

"""
            ]
        , title2 "How did you choose the vertex positions?"
        , paragraph []
            [ text """

I sampled them uniformly at random from the ground space, i.e., the
probability for a vertex to land in a given region is proportional to
the area of that region.

"""
            ]
        , paragraph []
            [ text
                (if hyperbolic then
                    """

You don't think the points are distributed uniformly?  Well they are,
although it seems that the points get denser towards the perimeter.
This comes from the distorted representation of the hyperbolic plane.
As the area of a disk grows exponentially with its radius, most of the
area of the ground space is close to its perimeter.  Thus, we get more
vertices there.

"""

                 else if euclidean then
                    """

This doesn't look very hyperbolic to you?  Well, you chose a rather
small radius for the ground space.  Locally the hyperbolic plane looks
like the Euclidean plane.  Thus, if you look at only a tiny disk in
the hyperbolic plane, you shouldn't expect to see much of the
hyperbolic stuff.

"""

                 else
                    """

The points seem somewhat off?  Like, not really uniformly distributed?
And what about hyperbolic geometry?  Wondering what is hyperbolic
about this?  You are somewhere between two extremes right now.  Try to
increase or decrease the ground space radius to get a clearer picture.

"""
                )
            ]
        , title2 "How do you choose the threshold distance?"
        , paragraph []
            [ wildcardText model """

I choose the threshold distance (radius of the gray disk) such that
the graph has average degree $avgDeg$.  If you increase the average
degree, I will increase the threshold distance.

"""
            ]
        , paragraph []
            [ wildcardText model """

If you increase the number of vertices, there will be less space
available for each vertex.  To keep the average degree at $avgDeg$, I
will decrease the threshold radius.

"""
            ]
        , paragraph []
            [ wildcardText model """

If you increase the ground space radius, the available space will grow
exponentially.  To keep the average degree at $avgDeg$, I will heavily
increase the threshold distance.

"""
            ]
        , title2 "How does this impact the graph structure?"
        , paragraph []
            [ wildcardText model
                (if hyperbolic then
                    """

The threshold distance is rather large ($thresholdRadiusRel$ of the
ground space radius), which leads to hierarchical structures.
Vertices are higher up in this hierarchy the closer they are to the
center.  Decrease the ground space radius and observe how the
structure changes.

"""

                 else if euclidean then
                    """

The threshold distance is small ($thresholdRadiusRel$ of the ground
space radius), which leads to grid-like structures as in Euclidean
unit disk graphs.  This is not surprising, as the ground space is
small and thus looks rather Euclidean.  Increase the ground space
radius and observe how the structure changes.

"""

                 else
                    """

You are somewhere between two extremes right now, which is why you get
some hierarchical and some grid-like structures.  Try to increase or
decrease the ground space radius to get a clearer picture.

"""
                )
            ]
        ]



-- DRAWING SUBROUTINES


canvas : Int -> List (Svg.Svg msg) -> Html msg
canvas canvasSize =
    let
        size =
            String.fromInt canvasSize
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


drawCircle : Float -> Float -> Float -> String -> Svg.Svg msg
drawCircle centerX centerY radius color =
    circle
        [ cx (String.fromFloat centerX)
        , cy (String.fromFloat centerY)
        , r (String.fromFloat radius)
        , Svg.Attributes.fill "none"
        , stroke color
        ]
        []


drawGroundSpace : Model -> Svg.Svg msg
drawGroundSpace model =
    let
        offset =
            toFloat model.canvasSize / 2
    in
    drawCircle offset offset offset "black"


drawThresholdRadius : Model -> Svg.Svg msg
drawThresholdRadius model =
    let
        offset =
            toFloat model.canvasSize / 2

        radius =
            offset * model.thresholdRadius / model.groundSpaceR
    in
    drawCircle offset offset radius "gray"
