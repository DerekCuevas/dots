module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time)
import Random exposing (Generator)
import Color exposing (Color)
import Array


-- CONFIG


dotCount =
    2000


dotWidth =
    20


updateDotsInterval =
    Time.second


colors =
    Array.fromList
        [ Color.lightRed
        , Color.lightOrange
        , Color.lightYellow
        , Color.lightGreen
        , Color.lightBlue
        , Color.lightPurple
        , Color.white
        ]



-- RANDOM GENERATORS


randomColorGenerator =
    Random.int 0 (Array.length colors - 1)
        |> Random.map
            (\n ->
                colors
                    |> Array.get n
                    |> Maybe.withDefault Color.white
            )


dotsGenerator =
    Random.list dotCount randomColorGenerator



-- COMMANDS


generateDots =
    Random.generate SetDots dotsGenerator



-- MODEL


init =
    { isTicking = True, time = Nothing, totalTicks = 0, dots = [] }
        ! [ generateDots ]



-- UPDATE


type Msg
    = ToggleIsTicking
    | Tick Time
    | SetDots (List Color)


update msg model =
    case msg of
        ToggleIsTicking ->
            { model | isTicking = not model.isTicking } ! []

        Tick time ->
            { model
                | time = Just time
                , totalTicks = model.totalTicks + 1
            }
                ! [ generateDots ]

        SetDots dots ->
            { model | dots = dots } ! []



-- SUBSCRIPTIONS


subscriptions model =
    if model.isTicking then
        Time.every updateDotsInterval Tick
    else
        Sub.none



-- VIEW


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
        "rgba("
            ++ toString red
            ++ ", "
            ++ toString green
            ++ ", "
            ++ toString blue
            ++ ", "
            ++ toString alpha
            ++ ")"


toPx : Int -> String
toPx x =
    toString x ++ "px"


viewDot color =
    let
        updateDotsIntervalInSeconds =
            Time.inSeconds updateDotsInterval

        viewStyle =
            style
                [ ( "backgroundColor", colorToString color )
                , ( "width", toPx dotWidth )
                , ( "height", toPx dotWidth )
                , ( "border-radius", toPx (dotWidth // 2) )
                , ( "margin", toPx (dotWidth // 8) )
                , ( "transition", "all " ++ toString updateDotsIntervalInSeconds ++ "s ease-out" )
                ]
    in
        div [ viewStyle ] []



-- PROGRAM


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
