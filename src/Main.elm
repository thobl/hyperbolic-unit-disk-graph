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
    , pointPairs : List ( Point, Point )
    , canvasSize : Float
    , groundSpaceR : Float
    , nrEdges : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { points = []
      , pointPairs = []
      , canvasSize = 1000
      , groundSpaceR = 8
      , nrEdges = 300
      }
    , Random.generate NewPoints (randomPoints 200)
    )


sortedPairs : Float -> List Point -> List ( Point, Point )
sortedPairs groundSpaceR points =
    let
        toH : Point -> PointH
        toH =
            toPointH groundSpaceR

        pairDist : ( Point, Point ) -> Float
        pairDist pair =
            dist (toH (Tuple.first pair)) (toH (Tuple.second pair))

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
    acosh (cosh p1.r * cosh p2.r - sinh p1.r * sinh p2.r * cos deltaPhi)



-- UPDATE


type Msg
    = NewPoints (List Point)
    | CanvasSizeChange String
    | GroundSpaceRChange String
    | AvgDegChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPoints newPoints ->
            ( { model
                | points = newPoints
                , pointPairs = sortedPairs model.groundSpaceR newPoints
              }
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
            in
            ( { model
                | groundSpaceR = 20 ^ inputF - 0.9999
                , pointPairs = sortedPairs model.groundSpaceR model.points
              }
            , Cmd.none
            )

        AvgDegChange input ->
            let 
                n = toFloat (List.length model.points)
                avgDeg = 
                    withDefault (2 * toFloat model.nrEdges / n) (String.toFloat input)
            in
            ( { model 
              | nrEdges = floor (avgDeg * n / 2)}
            , Cmd.none)



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
    let 
        n = toFloat (List.length model.points)
        avgDeg = 2 * toFloat model.nrEdges / n
    in
    div []
        [ div []
            [ slider 200 1200 1 model.canvasSize CanvasSizeChange
            , text "Canvas Size"
            ]
        ,div []
            [ slider 2 16 0.2 avgDeg AvgDegChange
            , text ("Avg Deg (" ++ String.fromFloat(avgDeg) ++ ")")
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
                        model.points
                    ++ List.map
                        (drawLine model.canvasSize model.groundSpaceR)
                        (List.take model.nrEdges model.pointPairs)
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


drawLine : Float -> Float -> ( Point, Point ) -> Svg.Svg msg
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
