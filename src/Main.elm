module Main exposing (main)

import Axis
import Browser
import Color
import Data exposing (GapminderEntries, GapminderSeries, WVEntries, WVWaves)
import Datasets.Gapminder exposing (gapminderData)
import Datasets.WVS exposing (wvsData)
import Dict exposing (Dict)
import Dict.Extra
import Element as UI
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import IntDict exposing (IntDict)
import Interpolation
import List.Extra
import Maybe.Extra
import Scale exposing (ContinuousScale)
import Set
import Statistics
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


w =
    900


h =
    600


padding =
    60


type alias Country =
    String


type alias Countries =
    List String


type alias Model =
    { currentSeries : GapminderEntries
    , currentValues : WVEntries
    , countries : Countries
    }


minimalYear =
    1999


maximalYear =
    2020


wvsYears =
    wvsData.year


init : Model
init =
    { currentSeries = gapminderData.gdp
    , currentValues = wvsData.confidenceInGovernment
    , countries = Dict.keys wvsData.year
    }


type Msg
    = NoOp


update : Msg -> Model -> Model
update cmd model =
    case cmd of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    UI.layout [ UI.width <| UI.fill, UI.height <| UI.fill ] <|
        UI.row [ UI.width <| UI.fill, UI.height <| UI.fill ]
            [ UI.html <| diagram model
            ]


selectCountries : Countries -> Dict String a -> Dict String a
selectCountries countries =
    Dict.Extra.keepOnly (Set.fromList countries)


xScale : WVEntries -> Countries -> ContinuousScale Float
xScale entries countries =
    entries
        |> selectCountries countries
        |> Dict.values
        |> List.concatMap (\{ wave5, wave6 } -> [ wave5, wave6 ])
        |> Statistics.extent
        |> Maybe.withDefault ( 0, 0.1 )
        |> Scale.linear ( 0, w - 2 * padding )


yScale : GapminderEntries -> Countries -> ContinuousScale Float
yScale entries countries =
    entries
        |> selectCountries countries
        |> Dict.values
        |> List.concatMap IntDict.values
        |> Statistics.extent
        |> Maybe.withDefault ( 0, 1.0 )
        |> Scale.linear ( h - 2 * padding, 0 )
        |> Scale.nice 4


drawSeries : WVEntries -> GapminderEntries -> Countries -> List (Svg Msg)
drawSeries values entries countries =
    []


type alias Point =
    { wvs : Float
    , gap : Float
    , t : Int
    }


type alias DisplayData =
    { country : String
    , points : List Point
    , sections : List ( Point, Point )
    }


pairwise : List a -> List ( a, a )
pairwise l =
    case Maybe.map2 List.Extra.zip (Just l) (List.tail l) of
        Just l_ ->
            l_

        Nothing ->
            []


listFromMaybe : Maybe a -> List a
listFromMaybe a =
    case a of
        Just a_ ->
            [ a_ ]

        Nothing ->
            []


intDictRange : Int -> Int -> IntDict a -> IntDict a
intDictRange start end d =
    d
        |> IntDict.partition (\i _ -> i >= start && i <= end)
        |> Tuple.first


makeSeries : WVEntries -> GapminderEntries -> Countries -> List DisplayData
makeSeries wvsValues gapminderValues countries =
    let
        getData : Country -> Maybe { wvsWaves : WVWaves Float, gapSeries : GapminderSeries, years : WVWaves Int, country : Country }
        getData country =
            Maybe.map4
                (\wvs gap ys c ->
                    { wvsWaves = wvs
                    , gapSeries = gap
                    , years = ys
                    , country = c
                    }
                )
                (Dict.get country wvsValues)
                (Dict.get country gapminderValues)
                (Dict.get country wvsYears)
                (Just country)

        addPoints : { wvsWaves : WVWaves Float, gapSeries : GapminderSeries, years : WVWaves Int, country : Country } -> { wvsWaves : WVWaves Float, gapSeries : GapminderSeries, years : WVWaves Int, country : Country, points : List Point }
        addPoints { wvsWaves, gapSeries, years, country } =
            { wvsWaves = wvsWaves
            , gapSeries = gapSeries
            , years = years
            , country = country
            , points = getPoints wvsWaves gapSeries years
            }

        getPoints : WVWaves Float -> GapminderSeries -> WVWaves Int -> List Point
        getPoints wvsWaves gapSeries years =
            Maybe.Extra.values
                [ Maybe.map3 (\wvs gap t -> { wvs = wvs, gap = gap, t = t })
                    (Just <| .wave5 wvsWaves)
                    (IntDict.get (.wave5 years) gapSeries)
                    (Just <| .wave5 years)
                , Maybe.map3 (\wvs gap t -> { wvs = wvs, gap = gap, t = t })
                    (Just <| .wave6 wvsWaves)
                    (IntDict.get (.wave6 years) gapSeries)
                    (Just <| .wave6 years)
                ]

        expandPoints : List Point -> List Point
        expandPoints pts =
            let
                add list first last =
                    ({ first | t = minimalYear } :: list) ++ [ { last | t = maximalYear } ]
            in
            case pts of
                [] ->
                    []

                _ ->
                    Maybe.map2 (add pts) (List.head pts) (List.Extra.last pts)
                        |> Maybe.withDefault []

        fillBetweenPoints : GapminderSeries -> ( Point, Point ) -> List Point
        fillBetweenPoints series ( p1, p2 ) =
            let
                between =
                    intDictRange p1.t p2.t series

                minWvs =
                    min p1.wvs p2.wvs

                maxWvs =
                    max p1.wvs p2.wvs

                makePoint ( minWvs_, maxWvs_ ) ( minT, maxT ) t v =
                    let
                        interpolate =
                            Interpolation.float minWvs_ maxWvs_
                    in
                    { wvs = interpolate (toFloat (t - minT) / toFloat (abs (minT - maxT)))
                    , gap = v
                    , t = t
                    }
            in
            IntDict.map (makePoint ( minWvs, maxWvs ) ( p1.t, p2.t )) between
                |> IntDict.values

        getSections gapSeries points =
            points
                |> expandPoints
                |> pairwise
                |> List.concatMap (fillBetweenPoints gapSeries)
                |> pairwise
    in
    countries
        |> List.filterMap getData
        |> List.map addPoints
        |> List.map
            (\{ gapSeries, country, points } ->
                { points = points
                , country = country
                , sections = getSections gapSeries points
                }
            )


drawPoints : WVEntries -> GapminderEntries -> Countries -> List (Svg Msg)
drawPoints values entries countries =
    let
        xScale_ =
            xScale values countries

        yScale_ =
            yScale entries countries

        drawPoint : Point -> Svg Msg
        drawPoint {wvs, gap} =
            circle
                [ r 5
                , fill <| Paint Color.lightBlue
                , stroke <| Paint Color.darkGray
                , strokeWidth 1
                , cx <| Scale.convert xScale_ wvs
                , cy <| Scale.convert yScale_ gap
                ]
                []

        seriesData = makeSeries values entries countries
    in
        List.concatMap (.points >> (List.map drawPoint)) seriesData


diagram : Model -> Html Msg
diagram model =
    svg [ viewBox 0 0 w h, HtmlAttr.width w, HtmlAttr.height h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ Axis.bottom [ Axis.tickCount 10 ] <| xScale model.currentValues model.countries ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ Axis.left [ Axis.tickCount 10 ] <| yScale model.currentSeries model.countries ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            drawSeries model.currentValues model.currentSeries model.countries
        , g [ transform [ Translate padding padding ], class [ "points" ] ] <|
            drawPoints model.currentValues model.currentSeries model.countries
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
