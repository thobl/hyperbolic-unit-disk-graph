module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (defaultThumb, labelLeft, slider)
import Html exposing (Html)
import List.Extra exposing (zip)
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


type alias HyperbolicPoint =
    -- Point in hyperbolic polar coordinates without.  Additionally sinh(r) and
    -- cosh(r) are preprocessed for faster distance computation.
    { r : Float
    , phi : Float
    , sinhR : Float
    , coshR : Float
    }


{-| A convenience method to easily initialize a hyperbolic point.
-}
initHyperbolicPoint : Float -> Float -> HyperbolicPoint
initHyperbolicPoint radius angle =
    { r = radius
    , phi = angle
    , sinhR = sinh radius
    , coshR = cosh radius
    }


type alias Point =
    -- Point in hyperbolic polar coordinates as well as canvas coordinates.
    { polar : HyperbolicPoint
    , x : Float
    , y : Float
    }

{-| This method allows us to sort a list of points in order of increasing
radius.
-}
radiallyIncreasing : Point -> Point -> Order
radiallyIncreasing p1 p2 =
    if p1.polar.r == p2.polar.r then
        EQ

    else if p1.polar.r < p2.polar.r then
        LT

    else
        GT


{-| Given the information about the `canvasSize` and `groundSpaceR`, converts
the `HyperbolicPoint` `point` to a `Point` that includes the canvas
representation.
-}
hyperbolicPointToPoint : Int -> Float -> HyperbolicPoint -> Point
hyperbolicPointToPoint canvasSize groundSpaceR point =
    let
        offset =
            toFloat canvasSize / 2
    in
    { polar = point
    , x = offset + offset * point.r / groundSpaceR * cos point.phi
    , y = offset + offset * point.r / groundSpaceR * sin point.phi
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
    { polar = initHyperbolicPoint r point.phi
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


{-| Computes the hyperbolic distance between two `HyperbolicPoint`s.
-}
dist : HyperbolicPoint -> HyperbolicPoint -> Float
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
    dist (Tuple.first pair).polar (Tuple.second pair).polar

{-| Rotates a `HyperbolicPoint` `point` around the origin by the passed
_positive_ `angle`.
-}
hyperbolicRotation : HyperbolicPoint -> Float -> HyperbolicPoint
hyperbolicRotation point angle =
    let
        addedAngle =
            point.phi + angle
    in
    if addedAngle < 2.0 * pi then
        initHyperbolicPoint point.r addedAngle

    else
        initHyperbolicPoint point.r (addedAngle - 2.0 * pi)

{-| Translates a `HyperbolicPoint` `point` by the passed `distance` along the
x-axis.
-}
hyperbolicTranslation : HyperbolicPoint -> Float -> HyperbolicPoint
hyperbolicTranslation point distance =
    -- The point doesn't change when the distance is 0.
    if distance == 0.0 then
        point

    else
    -- We first deal with the cases where the point lies on the x-axis.
    if
        point.phi == 0.0
    then
        initHyperbolicPoint (abs (point.r + distance))
            (if point.r + distance < 0.0 then
                pi

             else
                0.0
            )

    else if point.phi == pi then
        initHyperbolicPoint (abs (point.r - distance))
            (if point.r - distance < 0.0 then
                0.0

             else
                pi
            )
        -- Now we consider the case where the point does not lie on the x-axis.

    else
        let
            referencePoint =
                initHyperbolicPoint (abs distance)
                    (if distance > 0.0 then
                        pi

                     else
                        0.0
                    )

            movingPoint =
                initHyperbolicPoint point.r
                    (if point.phi > pi then
                        2.0 * pi - point.phi

                     else
                        point.phi
                    )

            radius =
                dist movingPoint referencePoint

            enumerator =
                (cosh (abs distance) * cosh radius) - cosh point.r

            denominator =
                sinh (abs distance) * sinh radius

            angle =
                acos (enumerator / denominator)

            adjustedAngle =
                if distance < 0.0 then
                    pi - angle

                else
                    angle

            mirroredAngle =
                if point.phi > pi then
                    2.0 * pi - adjustedAngle

                else
                    adjustedAngle
        in
        initHyperbolicPoint radius mirroredAngle



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
                    0
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
            { model | n = round n } |> updatePoints |> noCmd

        InputCanvasSize size ->
            { model | canvasSize = round size } |> updatePoints |> noCmd

        InputAvgDeg avgDeg ->
            { model | avgDeg = avgDeg } |> updateThresholdR |> noCmd

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
                            :: List.map (\x -> drawLine x "black") (List.take nrEdges model.pointPairs)
                            ++ drawHyperbolicCircle model (representativePoint model) model.thresholdRadius "red"
                            ++ List.map (\x -> drawPoint x "black") model.points
                            ++ [ drawPoint (representativePoint model) "red" ]
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


drawPoint : Point -> String -> Svg.Svg msg
drawPoint point color =
    circle
        [ cx (String.fromFloat point.x)
        , cy (String.fromFloat point.y)
        , r "3"
        , Svg.Attributes.fill color
        , stroke color
        ]
        []


drawLine : ( Point, Point ) -> String -> Svg.Svg msg
drawLine points color =
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
        , stroke color
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


{-| Returns a list of angles at at which we sample circle points. The closer we
get to an angle of pi, the more fine-grained should the sampling be.
The higher `nrSamples`, the more angles are added closer to pi.
-}
hyperbolicCircleSamples : Int -> List Float
hyperbolicCircleSamples nrSamples =
    let
        -- We start with a set of uniformly distributed points.  This ensures
        -- that we get nice round circles all around and not only close to pi.
        uniformAngles =
            List.map (\x -> toFloat x / 180.0 * Basics.pi) (List.range 0 359)

        -- The angles whose consecutive distances get smaller as we approach pi.
        detailAngles =
            List.map
                (\x ->
                    let
                        value =
                            toFloat x
                    in
                    Basics.pi * value / sqrt (1 + (value * value))
                )
                (List.range 0 nrSamples)

        -- When going from pi to 2 pi, we basically do the same thing but in
        -- reverse.  We always want to be fine close to pi.
        invertedDetailAngles =
            List.map (\x -> 2.0 * Basics.pi - x) (List.reverse (List.drop 1 detailAngles))
    in
    -- Combine all angles.
    List.sort (uniformAngles ++ detailAngles ++ invertedDetailAngles)


{-| Returns the SVG elements that represent a circle of radius `radius` centered
at the `point`. The element will have the passed `color`.
-}
drawHyperbolicCircle : Model -> Point -> Float -> String -> List (Svg.Svg msg)
drawHyperbolicCircle model point radius color =
    let
        -- We adjust the number of samples used for drawing the circle depending
        -- on the current `model.thresholdRadius`.  For a small radius, we are
        -- rather Euclidean and do not need many sample points.
        nrSamples =
            Basics.ceiling ((model.thresholdRadius / model.groundSpaceR) * (model.thresholdRadius / model.groundSpaceR) * 1500)

        -- The angles around the circle between which we draw lines.
        angles =
            hyperbolicCircleSamples nrSamples

        -- The points on the circle between which the lines are drawn.
        points =
            List.map (\angle -> initHyperbolicPoint radius angle) angles

        -- So far the circle was centered at the origin.  We now move the circle
        -- to the passed `point`, by first translating it horizontally such that
        -- the radial coordinate of the circle center matches the radial
        -- coordinate of the `point`...
        translatedPoints =
            List.map (\hyperbolicPoint -> hyperbolicTranslation hyperbolicPoint point.polar.r) points

        -- ... and then we rotate such that the angular coordinate of the circle
        -- center matches the angular coordinate of the `point`.
        rotatedPoints =
            List.map (\hyperbolicPoint -> hyperbolicRotation hyperbolicPoint point.polar.phi) translatedPoints

        -- So far these points were purely hyperbolic.  We now convert them to
        -- be able to draw them on the canvas.
        convertedPoints =
            List.map (\hyperbolicPoint -> hyperbolicPointToPoint model.canvasSize model.groundSpaceR hyperbolicPoint) rotatedPoints

        -- Now we have to get the lines between consecutive points on the
        -- circle.  To this end, we shift the points one to the circle
        -- cyclically by 1.
        shiftedPoints =
            List.drop 1 (List.Extra.cycle (List.length convertedPoints + 1) convertedPoints)

        -- Finally, we get the pairs of consecutive points
        pointPairs =
            List.Extra.zip convertedPoints shiftedPoints
    in
    -- List.map drawLine (List.Extra.zip pointPairs (List.repeat (List.length pointPairs) color))
    List.map (\x -> drawLine x color) pointPairs


drawGroundSpace : Model -> Svg.Svg msg
drawGroundSpace model =
    let
        offset =
            toFloat model.canvasSize / 2
    in
    drawCircle offset offset offset "black"


{-| Given the model, determines the point around which the hyperbolic circle
should be drawn that represents the threshold radius.
-}
representativePoint : Model -> Point
representativePoint model =
    let
        sortedPoints =
            List.sortWith radiallyIncreasing model.points
    in
    case List.Extra.getAt 9 sortedPoints of
        Nothing ->
            toPoint model.canvasSize model.groundSpaceR (PointVirt 0 0)

        Just p ->
            p
