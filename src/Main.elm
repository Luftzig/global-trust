module Main exposing (main)

import Axis
import Basics.Extra exposing (inDegrees)
import Browser
import Color exposing (Color)
import Color.Manipulate exposing (scaleHsl)
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
import Round
import Scale exposing (ContinuousScale)
import Set exposing (Set)
import Statistics
import String exposing (fromFloat, fromInt)
import TypedSvg exposing (circle, defs, g, marker, rect, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InEm
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, r, strokeWidth)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events
import TypedSvg.Types exposing (Paint(..), Transform(..))


w =
    900


h =
    600


padding =
    60


type alias Country =
    { name : String
    , color : Color
    }


type alias Countries =
    Set String


type alias Selector from to =
    { title : String
    , shortLabel : String
    , accessor : from -> to
    , lowLabel : String
    , highLabel : String
    }


type alias Model =
    { countries : Countries
    , gapminderSelector : Selector GapminderData GapminderEntries
    , wvsSelector : Selector WorldValuesData WVEntries
    , timeExtrapolation : Int
    , tooltip :
        Maybe
            { country : Country
            , position : ( Float, Float )
            , point : Point
            }
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
    [ { title = "Corruption Index", accessor = .corruption, shortLabel = "corrupt.", lowLabel = "more corrupt", highLabel = "less corrupt" }
    , { title = "GDP, in 2011 Dollars", accessor = .gdp, shortLabel = "GDP", lowLabel = "", highLabel = "$" }
    , { title = "Gini Index", accessor = .gini, shortLabel = "gini", lowLabel = "more equal", highLabel = "less equal" }
    , { title = "Murders, per 100,000", accessor = .murders, shortLabel = "murders", lowLabel = "", highLabel = "per 100k" }
    , { title = "% Below Poverty", accessor = .poverty, shortLabel = "% poor", lowLabel = "", highLabel = "%" }
    ]


valuesSelectors : List (Selector WorldValuesData WVEntries)
valuesSelectors =
    [ { accessor = .trustInNeighbors, title = "Trust in Neighbors", shortLabel = "trust: neighbors", lowLabel = "less trust", highLabel = "more trust" }
    , { accessor = .trustInPeopleYouKnow, title = "Trust in the People You Know", shortLabel = "trust: familiar", lowLabel = "less trust", highLabel = "more trust" }
    , { accessor = .trustInNewPeople, title = "Trust in People You Just Met", shortLabel = "trust: new people", lowLabel = "less trust", highLabel = "more trust" }
    , { accessor = .trustInDifferentNationality, title = "Trust in People of Different Nationality", shortLabel = "trust: diff. nation", lowLabel = "less trust", highLabel = "more trust" }
    , { accessor = .trustInDifferentReligion, title = "Trust in People of Different Religion", shortLabel = "trust: diff. religion", lowLabel = "less trust", highLabel = "more trust" }
    , { accessor = .confidenceInArmedForces, title = "Confidence in the Armed Forces", shortLabel = "confidence: armed forces", lowLabel = "less confidence", highLabel = "more confidence" }
    , { accessor = .confidenceInChurches, title = "Confidence in Religious Institutions", shortLabel = "confidence: religion", lowLabel = "less confidence", highLabel = "more confidence" }
    , { accessor = .confidenceInMajorCompanies, title = "Confidence in Major Companies", shortLabel = "confidence: companies", lowLabel = "less confidence", highLabel = "more confidence" }
    , { accessor = .confidenceInCourts, title = "Confidence in the Courts", shortLabel = "confidence: courts", lowLabel = "less confidence", highLabel = "more confidence" }
    , { accessor = .confidenceInGovernment, title = "Confidence in the Government", shortLabel = "confidence: government", lowLabel = "less confidence", highLabel = "more confidence" }
    , { accessor = .confidenceInParliament, title = "Confidence in the Parliament", shortLabel = "confidence: parliament", lowLabel = "less confidence", highLabel = "more confidence" }
    , { accessor = .confidenceInPolice, title = "Confidence in the Police", shortLabel = "confidence: police", lowLabel = "less confidence", highLabel = "more confidence" }
    , { accessor = .confidenceInPress, title = "Confidence in the Press", shortLabel = "confidence: press", lowLabel = "less confidence", highLabel = "more confidence" }
    ]


colors =
    { southAmerica = Color.orange
    , northAmerica = Color.red
    , europe = Color.blue
    , asia = Color.green
    , africa = Color.purple
    , oceania = Color.lightBrown
    }


allCountries : List Country
allCountries =
    [ { name = "Argentina", color = colors.southAmerica }
    , { name = "Australia", color = colors.oceania }
    , { name = "Brazil", color = colors.southAmerica }
    , { name = "Chile", color = colors.southAmerica }
    , { name = "China", color = colors.asia }
    , { name = "Colombia", color = colors.southAmerica }
    , { name = "Cyprus", color = colors.europe }
    , { name = "Georgia", color = colors.europe }
    , { name = "Germany", color = colors.europe }
    , { name = "Ghana", color = colors.africa }
    , { name = "India", color = colors.asia }
    , { name = "Jordan", color = colors.asia }
    , { name = "Malaysia", color = colors.asia }
    , { name = "Mexico", color = colors.northAmerica }
    , { name = "Morocco", color = colors.africa }
    , { name = "Netherlands", color = colors.europe }
    , { name = "Peru", color = colors.southAmerica }
    , { name = "Poland", color = colors.europe }
    , { name = "Romania", color = colors.europe }
    , { name = "Russia", color = colors.asia }
    , { name = "Slovenia", color = colors.europe }
    , { name = "South Africa", color = colors.africa }
    , { name = "South Korea", color = colors.asia }
    , { name = "Spain", color = colors.europe }
    , { name = "Sweden", color = colors.europe }
    , { name = "Taiwan", color = colors.asia }
    , { name = "Thailand", color = colors.asia }
    , { name = "Trinidad", color = colors.southAmerica }
    , { name = "Turkey", color = colors.asia }
    , { name = "Ukraine", color = colors.europe }
    , { name = "United States", color = colors.northAmerica }
    , { name = "Uruguay", color = colors.southAmerica }
    ]


init : Model
init =
    { countries = Set.fromList <| Dict.keys wvsData.year
    , gapminderSelector = List.head gapminderSelectors |> Maybe.withDefault { title = "", accessor = .gdp, shortLabel = "", lowLabel = "", highLabel = "" }
    , wvsSelector = List.head valuesSelectors |> Maybe.withDefault { accessor = .trustInNewPeople, title = "", shortLabel = "", lowLabel = "", highLabel = "" }
    , timeExtrapolation = 10
    , tooltip = Nothing
    }


type Msg
    = SelectGapminderEntries (Selector GapminderData GapminderEntries)
    | ToggleCountry Country
    | SelectAllCountries
    | DeselectAllCountries
    | SelectWVSEntries (Selector WorldValuesData WVEntries)
    | ShowTooltip Country ( Float, Float ) Point
    | HideTooltip


update : Msg -> Model -> Model
update cmd model =
    case cmd of
        SelectGapminderEntries selector ->
            { model | gapminderSelector = selector }

        ToggleCountry country ->
            { model
                | countries =
                    if Set.member country.name model.countries then
                        Set.remove country.name model.countries

                    else
                        Set.insert country.name model.countries
            }

        SelectWVSEntries selector ->
            { model | wvsSelector = selector }

        SelectAllCountries ->
            { model | countries = Set.fromList <| List.map .name allCountries }

        DeselectAllCountries ->
            { model | countries = Set.empty }

        ShowTooltip country position point ->
            { model | tooltip = Just { country = country, position = position, point = point } }

        HideTooltip ->
            { model | tooltip = Nothing }


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
        |> UI.column [ UI.spacing 4 ]


countryButton : Msg -> Country -> Bool -> UI.Element Msg
countryButton msg country active =
    UIInput.button
        [ UIBorder.rounded 5
        , UIBorder.color <| colorToUi country.color
        , UIBorder.width 2
        , UI.padding 4
        , if active then
            UIBackground.color <| colorToUi country.color

          else
            UIBackground.color <| UI.rgba 1.0 1.0 1.0 0.0
        , UIFont.size 10
        ]
        { onPress = Just msg
        , label = UI.text country.name
        }


countriesSelector { countries } =
    let
        buttonsProps =
            [ UIFont.size 10
            , UIFont.underline
            ]
    in
    allCountries
        |> List.map
            (\c ->
                countryButton (ToggleCountry c) c (Set.member c.name countries)
            )
        |> List.append
            [ UI.row [ UI.spaceEvenly, UI.spacing 5 ]
                [ UIInput.button buttonsProps { label = UI.text "All", onPress = Just SelectAllCountries }
                , UIInput.button buttonsProps { label = UI.text "None", onPress = Just DeselectAllCountries }
                ]
            ]
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
    { country : Country
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
                (Dict.get country.name wvsValues)
                (Dict.get country.name gapminderValues)
                (Dict.get country.name wvsYears)
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
    in
    allCountries
        |> List.filter (\c -> Set.member c.name countries)
        |> List.filterMap getData
        |> List.map addPoints
        |> List.map
            (\{ gapSeries, country, points } ->
                { points = points
                , country = country
                , segments = getSections gapSeries points
                }
            )


hoverBox : ( String, String ) -> { country : Country, position : ( Float, Float ), point : Point } -> Svg Msg
hoverBox ( wvsLabel, gapLabel ) { country, position, point } =
    let
        ( x, y ) =
            position

        adjustBackground =
            scaleHsl { lightnessScale = 0.3, saturationScale = 0.3, alphaScale = -0.2 }

        atRow n =
            TypedSvg.Attributes.InEm.y (1.2 * n)

        textWidth =
            TypedSvg.Attributes.InEm.textLength 2.5

        rowStart =
            TypedSvg.Attributes.InPx.x 4

        rowEnd =
            TypedSvg.Attributes.InEm.x 10

        rowEndAnchor =
            TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorEnd
    in
    g [ transform [ Translate x (y - 10) ] ]
        [ rect
            [ TypedSvg.Attributes.fill <| Paint <| adjustBackground country.color
            , TypedSvg.Attributes.stroke <| Paint Color.lightGrey
            , TypedSvg.Attributes.InPx.strokeWidth 1
            , TypedSvg.Attributes.InEm.width 5
            , TypedSvg.Attributes.InEm.height 3
            ]
            []
        , TypedSvg.text_ [ atRow 1, fontSize 9, rowStart ] [ text country.name ]
        , TypedSvg.text_ [ atRow 2, fontSize 9, rowStart ] [ text <| wvsLabel ++ ":" ]
        , TypedSvg.text_ [ atRow 4, fontSize 9, rowStart ] [ text <| gapLabel ++ ":" ]
        , TypedSvg.text_ [ atRow 5, fontSize 9, rowStart ] [ text <| "year:" ]
        , TypedSvg.text_ [ atRow 3, fontSize 9, rowEnd, rowEndAnchor ] [ text <| Round.round 4 point.wvs ]
        , TypedSvg.text_ [ atRow 4, fontSize 9, rowEnd, rowEndAnchor ] [ text <| Round.round 4 point.gap ]
        , TypedSvg.text_ [ atRow 5, fontSize 9, rowEnd, rowEndAnchor ] [ text <| fromInt point.t ]
        ]


drawPoints :
    List DisplayData
    -> ContinuousScale Float
    -> ContinuousScale Float
    -> List (Svg Msg)
drawPoints seriesData xScale_ yScale_ =
    let
        drawPoint : Country -> Point -> Svg Msg
        drawPoint country ({ wvs, gap } as point) =
            let
                x =
                    Scale.convert xScale_ wvs

                y =
                    Scale.convert yScale_ gap
            in
            circle
                [ r 5
                , fill <| Paint country.color
                , stroke <| Paint Color.darkGray
                , strokeWidth 1
                , cx <| x
                , cy <| y
                , TypedSvg.Events.onMouseEnter <| ShowTooltip country ( x, y ) point
                , TypedSvg.Events.onMouseLeave <| HideTooltip
                ]
                []
    in
    seriesData
        |> List.concatMap (\series -> List.map (drawPoint series.country) series.points)


segment : Country -> Float -> ( Float, Float ) -> ( Float, Float ) -> Svg Msg
segment country relative ( x1, y1 ) ( x2, y2 ) =
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
            country.color

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

        drawSegments_ country =
            preparePoints
                >> pairwise
                >> List.map (\( ( sx, sy, st ), ( ex, ey, _ ) ) -> segment country st ( sx, sy ) ( ex, ey ))
                >> g []

        drawSeries : DisplayData -> Svg Msg
        drawSeries { segments, country } =
            drawSegments_ country segments
    in
    data
        |> List.map drawSeries


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
            [ Axis.bottom [ Axis.tickCount 10 ] <| xScale_
            , TypedSvg.text_
                [ transform [ Translate 0 30 ]
                , TypedSvg.Attributes.InPx.fontSize 12
                , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorStart
                ]
                [ text model.wvsSelector.lowLabel ]
            , TypedSvg.text_
                [ transform [ Translate (h + 180) 30 ]
                , TypedSvg.Attributes.InPx.fontSize 12
                , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorEnd
                ]
                [ text model.wvsSelector.highLabel ]
            ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ Axis.left [ Axis.tickCount 10 ] <| yScale_
            , TypedSvg.text_
                [ transform [ Translate 0 -10 ]
                , TypedSvg.Attributes.InPx.fontSize 11
                , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorEnd
                ]
                [ text model.gapminderSelector.highLabel ]
            , TypedSvg.text_
                [ transform [ Translate 0 (h - 105) ]
                , TypedSvg.Attributes.InPx.fontSize 11
                , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorEnd
                ]
                [ text model.gapminderSelector.lowLabel ]
            ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            drawSegments seriesData xScale_ yScale_
        , g [ transform [ Translate padding padding ], class [ "points" ] ] <|
            drawPoints seriesData xScale_ yScale_
        , model.tooltip
            |> Maybe.map (hoverBox ( model.wvsSelector.shortLabel, model.gapminderSelector.shortLabel ))
            |> Maybe.withDefault (text "")
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
