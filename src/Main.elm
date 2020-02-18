module Main exposing (main)

import Axis
import Browser
import Color
import Data exposing (GapminderEntries, GapminderSeries, WVEntries)
import Datasets.Gapminder exposing (gapminderData)
import Datasets.WVS exposing (wvsData)
import Dict exposing (Dict)
import Dict.Extra
import Element as UI
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import IntDict
import Scale exposing (ContinuousScale)
import Set
import Statistics
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InEm exposing (r, strokeWidth)
import TypedSvg.Attributes.InPx exposing (cx, cy)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


w =
    900


h =
    600


padding =
    60


type alias SelectedCountries =
    List String


type alias Model =
    { currentSeries : GapminderEntries
    , currentValues : WVEntries
    , countries : SelectedCountries
    }


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
    UI.layout [UI.width <| UI.fill, UI.height <| UI.fill ] <|
        UI.row [UI.width <| UI.fill, UI.height <| UI.fill]
            [ UI.html <| diagram model
            ]


selectCountries : SelectedCountries -> Dict String a -> Dict String a
selectCountries countries =
    Dict.Extra.keepOnly (Set.fromList countries)


xScale : WVEntries -> SelectedCountries -> ContinuousScale Float
xScale entries countries =
    entries
        |> selectCountries countries
        |> Dict.values
        |> List.concatMap (\{ wave5, wave6 } -> [ wave5, wave6 ])
        |> Statistics.extent
        |> Maybe.withDefault ( 0, 0.1 )
        |> Scale.linear ( 0, w - 2 * padding )


yScale : GapminderEntries -> SelectedCountries -> ContinuousScale Float
yScale entries countries =
    entries
        |> selectCountries countries
        |> Dict.values
        |> List.concatMap IntDict.values
        |> Statistics.extent
        |> Maybe.withDefault ( 0, 1.0 )
        |> Scale.linear ( h - 2 * padding, 0 )
        |> Scale.nice 4


drawSeries : WVEntries -> GapminderEntries -> SelectedCountries -> List (Svg Msg)
drawSeries values entries countries =
    []


drawPoints : WVEntries -> GapminderEntries -> SelectedCountries -> List (Svg Msg)
drawPoints values entries countries =
    let
        xScale_ =
            xScale values countries

        yScale_ =
            yScale entries countries

        drawPoint {year, value, gapValue, country} =
            circle
                [ r 0.5
                , fill <| Paint Color.black
                , stroke <| Paint Color.darkGray
                , strokeWidth 0.1
                , cx <| Scale.convert xScale_ value
                , cy <| Scale.convert yScale_ gapValue
                ]
                []

        allJust { year, value, gapValue, country } =
            case ( year, value, gapValue ) of
                ( Just y, Just v, Just (Just gv) ) ->
                    Just { year = y, value = v, gapValue = gv, country = country }

                _ ->
                    Nothing

        dataPoints : List {year : Int, value: Float, gapValue: Float, country: String}
        dataPoints =
            countries
                |> List.concatMap
                    (\country ->
                        [ { year = Maybe.map .wave5 <| Dict.get country wvsYears
                          , value = Maybe.map .wave5 <| Dict.get country values
                          , country = country
                          }
                        , { year = Maybe.map .wave6 <| Dict.get country wvsYears
                          , value = Maybe.map .wave6 <| Dict.get country values
                          , country = country
                          }
                        ]
                    )
                |> List.map
                    (\{ country, year, value } ->
                        { gapValue =
                            Dict.get country entries
                                |> Maybe.map (IntDict.get (Maybe.withDefault 0 year))
                        , country = country
                        , year = year
                        , value = value
                        }
                    )
                |> List.filterMap allJust

        points =
            List.map drawPoint dataPoints
    in
    points


diagram : Model -> Html Msg
diagram model =
    svg [ viewBox 0 0 w h, HtmlAttr.width w, HtmlAttr.height h]
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
