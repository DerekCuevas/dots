module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Random
import Color
import Array


type alias Model =
    { isTicking : Bool
    , time : Maybe Time
    , total : Int
    , length : Int
    }


init =
    { isTicking = True, time = Nothing, total = 0, length = 0 } ! []


type Msg
    = ToggleIsTicking
    | Tick Time
    | SetRandomListLength Int


randomListLengthGenerator =
    Random.int 0 1000


update msg model =
    case msg of
        ToggleIsTicking ->
            { model | isTicking = not model.isTicking } ! []

        Tick time ->
            { model
                | time = Just time
                , total = model.total + 1
            }
                ! [ Random.generate
                        SetRandomListLength
                        randomListLengthGenerator
                  ]

        SetRandomListLength length ->
            { model | length = length } ! []


tickInterval =
    Time.second / 2


subscriptions model =
    if model.isTicking then
        Time.every tickInterval Tick
    else
        Sub.none


view model =
    let
        time =
            model.time
                |> Maybe.map toString
                |> Maybe.withDefault "N/A"

        numbers =
            List.range 0 model.length
    in
        div []
            [ button [ onClick ToggleIsTicking ]
                [ text
                    (if model.isTicking then
                        "Stop"
                     else
                        "Start"
                    )
                ]
            , div []
                [ text ("Time: " ++ time) ]
            , div []
                [ text ("Total Ticks: " ++ toString model.total) ]
            , div []
                [ text ("Length: " ++ toString model.length) ]
            , div []
                [ div [ style [ ( "display", "flex" ), ( "flex-wrap", "wrap" ) ] ] <|
                    List.map (viewListItem model.length) numbers
                ]
            ]


lightColors =
    Array.fromList
        [ Color.lightRed
        , Color.lightOrange
        , Color.lightYellow
        , Color.lightGreen
        , Color.lightBlue
        , Color.lightPurple
        ]


getColorAt n =
    let
        index =
            n % Array.length lightColors
    in
        Array.get index lightColors


colorToString color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgba(" ++ toString red ++ "," ++ toString green ++ ", " ++ toString blue ++ ", " ++ toString alpha ++ ")"


viewListItem length n =
    let
        color =
            getColorAt (length * n)
                |> Maybe.map colorToString
                |> Maybe.withDefault ""

        tickIntervalInSeconds =
            Time.inSeconds tickInterval

        viewStyle =
            style
                [ ( "backgroundColor", color )
                , ( "color", "white" )
                , ( "width", "50px" )
                , ( "height", "50px" )
                , ( "border-radius", "25px" )
                , ( "margin", "5px" )
                , ( "transition", "all " ++ toString tickIntervalInSeconds ++ "s ease-out" )
                ]
    in
        div [ viewStyle ] []


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
