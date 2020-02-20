module Main exposing (main)

import Axis
import Basics.Extra exposing (inDegrees)
import Browser
import Color exposing (Color)
import Data exposing (GapminderEntries, GapminderSeries, WVEntries, WVWaves)
import Datasets.Gapminder exposing (gapminderData)
import Datasets.WVS exposing (wvsData)
import Dict exposing (Dict)
import Dict.Extra
import Element as UI
import Element.Background as UIBackground
import Element.Border as UIBorder
import Element.Events
import Element.Font as UIFont
import Element.Input as UIInput
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import IntDict exposing (IntDict)
import Interpolation
import List.Extra
import Maybe.Extra
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Set exposing (Set)
import Shape
import Statistics
import TypedSvg exposing (circle, defs, g, line, marker, svg)
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


type alias Model =
    { currentSeries : GapminderEntries
    , currentValues : WVEntries
    , countries : Countries
    }


minimalYear =
    1999


maximalYear =
    2020


arrowMarkerId =
    "arrow-marker-id"


wvsYears =
    wvsData.year


init : Model
init =
    { currentSeries = gapminderData.gdp
    , currentValues = wvsData.confidenceInGovernment
    , countries = Set.fromList <| Dict.keys wvsData.year
    }


type Msg
    = ChangeGapminderSeries GapminderEntries
    | ToggleCountry Country
    | ToggleWVS WVEntries


update : Msg -> Model -> Model
update cmd model =
    case cmd of
        ChangeGapminderSeries series ->
            { model | currentSeries = series }

        ToggleCountry country ->
            { model
                | countries =
                    if Set.member country model.countries then
                        Set.remove country model.countries

                    else
                        Set.insert country model.countries
            }

        ToggleWVS wVEntries ->
            { model | currentValues = wVEntries }


view : Model -> Html Msg
view model =
    UI.layout [ UI.width <| UI.fill, UI.height <| UI.fill ] <|
        UI.column [ UI.width <| UI.fill, UI.height <| UI.fill, UI.centerX ]
            [ UI.el [ UI.centerX, UIFont.size 24 ] <| UI.text "Global Trust"
            , UI.row []
                [ gapminderSelector model
                , UI.html <| diagram model
                , countriesSelector model
                ]
            , valuesSelector model
            ]


gapminderSelectors =
    [ { title = "Corruption Index", accessor = .corruption }
    , { title = "GDP", accessor = .gdp }
    , { title = "GINI Index", accessor = .gini }
    , { title = "Murders, per 100,000", accessor = .murders }
    , { title = "% Below Poverty", accessor = .poverty }
    ]


colorToUi =
    UI.fromRgb << Color.toRgba


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
        ]
        { onPress = Just msg
        , label = UI.text label
        }


gapminderSelector : Model -> UI.Element Msg
gapminderSelector model =
    gapminderSelectors
        |> List.map
            (\{ title, accessor } ->
                button
                    (ChangeGapminderSeries <| accessor gapminderData)
                    title
                    (accessor gapminderData == model.currentSeries)
            )
        |> UI.column []


countriesSelector { countries } =
    Dict.keys wvsYears
        |> List.map
            (\c ->
                UIInput.button
                    [ UIBorder.rounded 5
                    , UIBorder.color <| colorToUi Color.lightBlue
                    , if Set.member c countries then
                        UIBackground.color <| colorToUi Color.lightBlue

                      else
                        UIBackground.color <| UI.rgba 1.0 1.0 1.0 0.0
                    ]
                    { label = UI.text c
                    , onPress = Just <| ToggleCountry c
                    }
            )
        |> UI.column []


valuesSelectors =
    [ { accessor = .trustInNeighbors, title = "Trust in Neighbors" }
    , { accessor = .trustInPeopleYouKnow, title = "Trust in the People You Know" }
    , { accessor = .trustInNewPeople, title = "Trust in People You Just Met" }
    , { accessor = .trustInDifferentNationality, title = "Trust in People of Different Nationality" }
    , { accessor = .trustInDifferentReligion, title = "Trust in People of Different Religion" }
    , { accessor = .confidenceInArmedForces, title = "Confidence in the Armed Forces" }
    , { accessor = .confidenceInChurches, title = "Confidence in Religious Institutions" }
    , { accessor = .confidenceInMajorCompanies, title = "Confidence in Major Companies" }
    , { accessor = .confidenceInCourts, title = "Confidence in the Courts" }
    , { accessor = .confidenceInGovernment, title = "Confidence in the Government" }
    , { accessor = .confidenceInParliament, title = "Confidence in the Parliament" }
    , { accessor = .confidenceInPolice, title = "Confidence in the Police" }
    , { accessor = .confidenceInPress, title = "Confidence in the Press" }
    ]


valuesSelector { currentValues } =
    valuesSelectors
        |> List.map
            (\{ accessor, title } ->
                button
                    (ToggleWVS <| accessor wvsData)
                    title
                    (currentValues == accessor wvsData)
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
            Scale.linear ( 0, 1 ) ( 0.1, 0.9 )

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
        xScale_ =
            xScale model.currentValues model.countries

        yScale_ =
            yScale model.currentSeries model.countries

        seriesData =
            makeSeries model.currentValues model.currentSeries model.countries
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
