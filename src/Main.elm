module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Random exposing (Generator)
import Color exposing (Color)
import Array


dotCount =
    300


updateDotsInterval =
    Time.second


type alias Model =
    { isTicking : Bool
    , time : Maybe Time
    , totalTicks : Int
    , dots : List Color
    }


init =
    { isTicking = True, time = Nothing, totalTicks = 0, dots = [] } ! []


type Msg
    = ToggleIsTicking
    | Tick Time
    | SetDots (List Color)


randomColorGenerator =
    let
        colors =
            Array.fromList
                [ Color.lightRed
                , Color.lightOrange
                , Color.lightYellow
                , Color.lightGreen
                , Color.lightBlue
                , Color.lightPurple
                ]
    in
        Random.int 0 (Array.length colors - 1)
            |> Random.map
                (\n ->
                    colors
                        |> Array.get n
                        |> Maybe.withDefault Color.white
                )


dotsGenerator =
    Random.list dotCount randomColorGenerator


update msg model =
    case msg of
        ToggleIsTicking ->
            { model | isTicking = not model.isTicking } ! []

        Tick time ->
            { model
                | time = Just time
                , totalTicks = model.totalTicks + 1
            }
                ! [ Random.generate SetDots dotsGenerator ]

        SetDots dots ->
            { model | dots = dots } ! []


subscriptions model =
    if model.isTicking then
        Time.every updateDotsInterval Tick
    else
        Sub.none


view model =
    let
        time =
            model.time
                |> Maybe.map toString
                |> Maybe.withDefault "N/A"
    in
        div []
            [ div []
                [ div [ style [ ( "display", "flex" ), ( "flex-wrap", "wrap" ) ] ] <|
                    List.map viewDot model.dots
                ]
            , button [ onClick ToggleIsTicking ]
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
                [ text ("Total Ticks: " ++ toString model.totalTicks) ]
            ]


colorToString color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgba(" ++ toString red ++ "," ++ toString green ++ ", " ++ toString blue ++ ", " ++ toString alpha ++ ")"


viewDot color =
    let
        updateDotsIntervalInSeconds =
            Time.inSeconds updateDotsInterval

        viewStyle =
            style
                [ ( "backgroundColor", colorToString color )
                , ( "color", "white" )
                , ( "width", "50px" )
                , ( "height", "50px" )
                , ( "border-radius", "25px" )
                , ( "margin", "5px" )
                , ( "transition", "all " ++ toString updateDotsIntervalInSeconds ++ "s ease-out" )
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
