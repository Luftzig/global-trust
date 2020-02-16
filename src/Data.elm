module Data exposing (..)

import Datasets.RawGapminder exposing (rawGapminderJson)
import Datasets.RawWVS exposing (wvsRawJson)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Json.Decode as Decode exposing (Decoder, float, int, maybe, succeed)
import Json.Decode.Pipeline exposing (required)
import String exposing (toInt)


type alias WorldValuesData =
    { trustInNeighbors : WVEntries
    , trustInPeopleYouKnow : WVEntries
    , trustInNewPeople : WVEntries
    , trustInDifferentNationality : WVEntries
    , trustInDifferentReligion : WVEntries
    , confidenceInArmedForces : WVEntries
    , confidenceInChurches : WVEntries
    , confidenceInMajorCompanies : WVEntries
    , confidenceInCourts : WVEntries
    , confidenceInGovernment : WVEntries
    , confidenceInParliament : WVEntries
    , confidenceInPolice : WVEntries
    , confidenceInPress : WVEntries
    , year : WVYear
    }


type alias WVWaves a =
    { wave5 : a, wave6 : a }


type alias WVEntries =
    Dict String (WVWaves Float)


type alias WVYear =
    Dict String (WVWaves Int)


decodeWorldValuesData : Decoder WorldValuesData
decodeWorldValuesData =
    succeed WorldValuesData
        |> required "trust: neighbors" decodeWVEntries
        |> required "trust: people you know" decodeWVEntries
        |> required "trust: new people" decodeWVEntries
        |> required "trust: people of different nationality" decodeWVEntries
        |> required "trust: people of different religion" decodeWVEntries
        |> required "confidence: armed forces" decodeWVEntries
        |> required "confidence: churches" decodeWVEntries
        |> required "confidence: major companies" decodeWVEntries
        |> required "confidence: the courts" decodeWVEntries
        |> required "confidence: the government" decodeWVEntries
        |> required "confidence: the parliament" decodeWVEntries
        |> required "confidence: the police" decodeWVEntries
        |> required "confidence: the press" decodeWVEntries
        |> required "year" decodeWVYear


decodeWVEntries : Decoder WVEntries
decodeWVEntries =
    Decode.dict decodeWVWavesFloat


decodeWVYear : Decoder WVYear
decodeWVYear =
    Decode.dict decodeWVWavesInt


decodeWVWavesFloat : Decoder (WVWaves Float)
decodeWVWavesFloat =
    Decode.succeed WVWaves
        |> required "wave5" float
        |> required "wave6" float


decodeWVWavesInt : Decoder (WVWaves Int)
decodeWVWavesInt =
    Decode.succeed WVWaves
        |> required "wave5" int
        |> required "wave6" int

decodeWVS = Decode.decodeString decodeWorldValuesData wvsRawJson

type alias GapminderData =
    { corruption : GapminderEntries
    , gdp: GapminderEntries
    , gini: GapminderEntries
    , murders: GapminderEntries
    , poverty: GapminderEntries
    }


type alias GapminderEntries = Dict String GapminderSeries

type alias GapminderSeriesRaw = List (Int, Float)

type alias GapminderSeries = IntDict Float

-- This code used for reading the input files
--decodeGapminderData : Decoder GapminderData
--decodeGapminderData =
--    succeed GapminderData
--        |> required "corruption" decodeGapminderEntries
--        |> required "gdp" decodeGapminderEntries
--        |> required "gini" decodeGapminderEntries
--        |> required "murders" decodeGapminderEntries
--        |> required "poverty" decodeGapminderEntries
--
--decodeGapminderEntries : Decoder GapminderEntries
--decodeGapminderEntries =
--    Decode.dict decodeGapminderSeries
--
--decodeGapminderSeries : Decoder (List (Int, Float))
--decodeGapminderSeries =
--    Decode.keyValuePairs (maybe float)
--    |> Decode.andThen decodePairs
--
--decodePairs : List (String, Maybe Float) -> Decoder (List (Int, Float))
--decodePairs ps =
--    let
--        maybeSecond (a, mb) = Maybe.map (\b -> (a, b)) mb
--        maybeFirst (ma, b) = Maybe.map (\a -> (a, b)) ma
--        onlyValid = List.filterMap maybeSecond ps
--        parsedYears = List.filterMap ((\(y, v) -> (toInt y, v)) >> maybeFirst) onlyValid
--
--    in succeed parsedYears
--
--
--getGapminderData = Decode.decodeString decodeGapminderData rawGapminderJson
--
--
