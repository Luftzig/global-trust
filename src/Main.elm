module Main exposing (main)

import Axis
import Basics.Extra exposing (inDegrees)
import Browser
import Color exposing (Color)
import Data exposing (GapminderData, GapminderEntries, GapminderSeries, WVEntries, WVWaves, WorldValuesData)
import Datasets.Gapminder exposing (gapminderData)
import Datasets.WVS exposing (wvsData)
import Dict exposing (Dict)
import Dict.Extra
import Element as UI
import Element.Background as UIBackground
import Element.Border as UIBorder
import Element.Font as UIFont
import Element.Input as UIInput
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import IntDict exposing (IntDict)
import List.Extra
import Maybe.Extra
import Scale exposing (ContinuousScale)
import Set exposing (Set)
import Statistics
import TypedSvg exposing (circle, defs, g, marker, svg)
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
    Set String


type alias Selector from to =
    { title : String
    , shortLabel : String
    , accessor : from -> to
    }


type alias Model =
    { countries : Countries
    , gapminderSelector : Selector GapminderData GapminderEntries
    , wvsSelector : Selector WorldValuesData WVEntries
    , timeExtrapolation : Int
    }


minimalYear =
    1999


maximalYear =
    2020


arrowMarkerId =
    "arrow-marker-id"


wvsYears =
    wvsData.year


gapminderSelectors : List (Selector GapminderData GapminderEntries)
gapminderSelectors =
    [ { title = "Corruption Index", accessor = .corruption, shortLabel = "corrupt." }
    , { title = "GDP, in 2011 Dollars", accessor = .gdp, shortLabel = "GDP" }
    , { title = "Gini Index", accessor = .gini, shortLabel = "gini" }
    , { title = "Murders, per 100,000", accessor = .murders, shortLabel = "murders" }
    , { title = "% Below Poverty", accessor = .poverty, shortLabel = "% poor" }
    ]


valuesSelectors : List (Selector WorldValuesData WVEntries)
valuesSelectors =
    [ { accessor = .trustInNeighbors, title = "Trust in Neighbors", shortLabel = "trust: neighbors" }
    , { accessor = .trustInPeopleYouKnow, title = "Trust in the People You Know", shortLabel = "trust: familiar" }
    , { accessor = .trustInNewPeople, title = "Trust in People You Just Met", shortLabel = "trust: new people" }
    , { accessor = .trustInDifferentNationality, title = "Trust in People of Different Nationality", shortLabel = "trust: diff. nation" }
    , { accessor = .trustInDifferentReligion, title = "Trust in People of Different Religion", shortLabel = "trust: diff. religion" }
    , { accessor = .confidenceInArmedForces, title = "Confidence in the Armed Forces", shortLabel = "confidence: armed forces" }
    , { accessor = .confidenceInChurches, title = "Confidence in Religious Institutions", shortLabel = "confidence: religion" }
    , { accessor = .confidenceInMajorCompanies, title = "Confidence in Major Companies", shortLabel = "confidence: companies" }
    , { accessor = .confidenceInCourts, title = "Confidence in the Courts", shortLabel = "confidence: courts" }
    , { accessor = .confidenceInGovernment, title = "Confidence in the Government", shortLabel = "confidence: government" }
    , { accessor = .confidenceInParliament, title = "Confidence in the Parliament", shortLabel = "confidence: parliament" }
    , { accessor = .confidenceInPolice, title = "Confidence in the Police", shortLabel = "confidence: police" }
    , { accessor = .confidenceInPress, title = "Confidence in the Press", shortLabel = "confidence: press" }
    ]


init : Model
init =
    { countries = Set.fromList <| Dict.keys wvsData.year
    , gapminderSelector = { title = "GDP, 2011 $", accessor = .gdp, shortLabel = "GDP $" }
    , wvsSelector = { accessor = .trustInNewPeople, title = "Trust in People You Just Met", shortLabel = "trust: new people" }
    , timeExtrapolation = 10
    }


type Msg
    = SelectGapminderEntries (Selector GapminderData GapminderEntries)
    | ToggleCountry Country
    | SelectWVSEntries (Selector WorldValuesData WVEntries)


update : Msg -> Model -> Model
update cmd model =
    case cmd of
        SelectGapminderEntries selector ->
            { model | gapminderSelector = selector }

        ToggleCountry country ->
            { model
                | countries =
                    if Set.member country model.countries then
                        Set.remove country model.countries

                    else
                        Set.insert country model.countries
            }

        SelectWVSEntries selector ->
            { model | wvsSelector = selector }


view : Model -> Html Msg
view model =
    UI.layout [ UI.width <| UI.fill, UI.height <| UI.fill ] <|
        UI.column [ UI.width <| UI.fill, UI.height <| UI.fill, UI.centerX, UI.padding 5 ]
            [ UI.el [ UI.centerX, UIFont.size 24 ] <| UI.text "Global Trust"
            , UI.row []
                [ UI.column []
                    [ UI.row []
                        [ gapminderSelector model
                        , UI.column []
                            [ drawTitle model
                            , UI.html <| diagram model
                            , valuesSelector model
                            ]
                        ]
                    ]
                , countriesSelector model
                ]
            ]


colorToUi =
    UI.fromRgb << Color.toRgba


drawTitle model =
    UI.row [ UI.width <| UI.fill, UI.centerX, UI.spaceEvenly ]
        [ UI.el [] <| UI.none
        , UI.el [ UI.centerX ] <| UI.text <| model.wvsSelector.title ++ " vs. " ++ model.gapminderSelector.title
        ]


button : Msg -> String -> Bool -> UI.Element Msg
button msg label active =
    UIInput.button
        [ UIBorder.rounded 5
        , UIBorder.color <| colorToUi Color.lightBlue
        , UIBorder.width 2
        , UI.padding 4
        , if active then
            UIBackground.color <| colorToUi Color.lightBlue

          else
            UIBackground.color <| UI.rgba 1.0 1.0 1.0 0.0
        , UIFont.size 12
        ]
        { onPress = Just msg
        , label = UI.text label
        }


gapminderSelector : Model -> UI.Element Msg
gapminderSelector model =
    gapminderSelectors
        |> List.map
            (\({ title, accessor } as selector) ->
                button
                    (SelectGapminderEntries selector)
                    title
                    (title == model.gapminderSelector.title)
            )
        |> UI.column []


countryButton : Msg -> String -> Bool -> UI.Element Msg
countryButton msg label active =
    UIInput.button
        [ UIBorder.rounded 5
        , UIBorder.color <| colorToUi Color.lightBlue
        , UIBorder.width 2
        , UI.padding 4
        , if active then
            UIBackground.color <| colorToUi Color.lightBlue

          else
            UIBackground.color <| UI.rgba 1.0 1.0 1.0 0.0
        , UIFont.size 10
        ]
        { onPress = Just msg
        , label = UI.text label
        }


countriesSelector { countries } =
    Dict.keys wvsYears
        |> List.map
            (\c ->
                countryButton (ToggleCountry c) c (Set.member c countries)
            )
        |> UI.column [ UI.spacing 2 ]


valuesSelector { wvsSelector } =
    valuesSelectors
        |> List.map
            (\({ accessor, title } as selector) ->
                button
                    (SelectWVSEntries selector)
                    title
                    (title == wvsSelector.title)
            )
        |> UI.wrappedRow [ UI.width <| UI.fill, UI.spacing 4 ]


selectCountries : Countries -> Dict String a -> Dict String a
selectCountries countries =
    Dict.Extra.keepOnly countries


extendScale ( minimal, maximal ) factor ( low, high ) =
    ( max minimal (low - (factor * (high - low))), min maximal (high + (factor * (high - low))) )


xScale : WVEntries -> Countries -> ContinuousScale Float
xScale entries countries =
    entries
        |> selectCountries countries
        |> Dict.values
        |> List.concatMap (\{ wave5, wave6 } -> [ wave5, wave6 ])
        |> Statistics.extent
        |> Maybe.map (extendScale ( 0, 4 ) 0.05)
        |> Maybe.withDefault ( 0, 4.0 )
        |> Scale.linear ( 0, w - 2 * padding )


yScale : GapminderEntries -> Countries -> ContinuousScale Float
yScale entries countries =
    entries
        |> selectCountries countries
        |> Dict.values
        |> List.concatMap IntDict.values
        |> Statistics.extent
        |> Maybe.map (extendScale ( 0, 100000 ) 0.1)
        |> Maybe.withDefault ( 0, 1.0 )
        |> Scale.linear ( h - 2 * padding, 0 )
        |> Scale.nice 4


type alias Point =
    { wvs : Float
    , gap : Float
    , t : Int
    }


type alias DisplayData =
    { country : String
    , points : List Point
    , segments : List Point
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
                    if p1.t < p2.t then
                        p1.wvs

                    else
                        p2.wvs

                maxWvs =
                    if p1.t < p2.t then
                        p2.wvs

                    else
                        p1.wvs

                makePoint ( minWvs_, maxWvs_ ) ( minT, maxT ) t v =
                    let
                        timeScale =
                            Scale.linear ( minWvs_, maxWvs_ ) ( toFloat minT, toFloat maxT )
                    in
                    { wvs = Scale.convert timeScale <| toFloat t
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
                |> List.sortBy .t
                |> Debug.log "sections"
    in
    countries
        |> Set.toList
        |> List.filterMap getData
        |> List.map addPoints
        |> List.map
            (\{ gapSeries, country, points } ->
                { points = points
                , country = country
                , segments = getSections gapSeries points
                }
            )


drawPoints : List DisplayData -> ContinuousScale Float -> ContinuousScale Float -> List (Svg Msg)
drawPoints seriesData xScale_ yScale_ =
    let
        drawPoint : Point -> Svg Msg
        drawPoint { wvs, gap } =
            circle
                [ r 5
                , fill <| Paint Color.lightBlue
                , stroke <| Paint Color.darkGray
                , strokeWidth 1
                , cx <| Scale.convert xScale_ wvs
                , cy <| Scale.convert yScale_ gap
                ]
                []
    in
    List.concatMap (.points >> List.map drawPoint) seriesData


segment : Float -> ( Float, Float ) -> ( Float, Float ) -> Svg Msg
segment relative ( x1, y1 ) ( x2, y2 ) =
    let
        width =
            4

        dx =
            x2 - x1

        dy =
            y2 - y1

        angle =
            atan2 dy dx

        baseColor =
            Color.blue

        hsl =
            Color.toHsla baseColor

        scale =
            Scale.linear ( 0.1, 0.7 ) ( 0, 1 )

        segmentColor =
            Color.fromHsla { hsl | lightness = Scale.convert scale relative, alpha = 0.5 }
    in
    TypedSvg.polygon
        [ TypedSvg.Attributes.points
            [ ( x1 + (width * sin angle), y1 - (width * cos angle) )
            , ( x2, y2 )
            , ( x1 - (width * sin angle), y1 + (width * cos angle) )
            , ( x1 + ((width / 2) * cos angle), y1 + ((width / 2) * sin angle) )
            ]
        , TypedSvg.Attributes.fill <| Paint segmentColor
        ]
        []


drawSegments : List DisplayData -> ContinuousScale Float -> ContinuousScale Float -> List (Svg Msg)
drawSegments data xScale_ yScale_ =
    let
        preparePoints : List Point -> List ( Float, Float, Float )
        preparePoints pts =
            let
                time =
                    pts
                        |> List.map (.t >> toFloat)
                        |> Statistics.extent
                        |> Maybe.withDefault ( 0, 0 )

                timeScale =
                    Scale.linear ( 0, 1 ) time

                scaleXYT : Point -> ( Float, Float, Float )
                scaleXYT p =
                    ( Scale.convert xScale_ p.wvs, Scale.convert yScale_ p.gap, Scale.convert timeScale <| toFloat p.t )
            in
            pts
                |> List.map scaleXYT
    in
    List.map .segments data
        |> List.map (List.sortBy .t)
        |> List.map (preparePoints >> pairwise >> List.map (\( ( sx, sy, st ), ( ex, ey, _ ) ) -> segment st ( sx, sy ) ( ex, ey )) >> g [])


diagram : Model -> Html Msg
diagram model =
    let
        currentWVS =
            model.wvsSelector.accessor wvsData

        currentGapminder =
            model.gapminderSelector.accessor gapminderData

        xScale_ =
            xScale currentWVS model.countries

        yScale_ =
            yScale currentGapminder model.countries

        seriesData =
            makeSeries currentWVS currentGapminder model.countries
    in
    svg [ viewBox 0 0 w h, HtmlAttr.width w, HtmlAttr.height h ]
        [ defs []
            [ marker
                [ TypedSvg.Attributes.id arrowMarkerId
                , viewBox 0 0 10 10
                , TypedSvg.Attributes.refX "5"
                , TypedSvg.Attributes.refY "5"
                , TypedSvg.Attributes.markerWidth <| TypedSvg.Types.Px 3
                , TypedSvg.Attributes.markerHeight <| TypedSvg.Types.Px 3
                , TypedSvg.Attributes.orient "auto-start-reverse"
                ]
                [ TypedSvg.path
                    [ TypedSvg.Attributes.d "M 0 0 L 10 5 L 0 10 z"
                    , stroke <| Paint Color.white
                    , strokeWidth 2
                    , fill PaintNone
                    ]
                    []
                ]
            ]
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ Axis.bottom [ Axis.tickCount 10 ] <| xScale_ ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ Axis.left [ Axis.tickCount 10 ] <| yScale_ ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            drawSegments seriesData xScale_ yScale_
        , g [ transform [ Translate padding padding ], class [ "points" ] ] <|
            drawPoints seriesData xScale_ yScale_
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
