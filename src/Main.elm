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
    99


dotWidth =
    150


updateDotsInterval =
    Time.second * 2


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


borderRadiusGenerator =
    Random.float 0 (dotWidth / 2)



-- COMMANDS


generateDots =
    Random.generate SetDots dotsGenerator


generateBorderRadius =
    Random.generate SetBorderRadius borderRadiusGenerator


generate =
    [ generateDots
    , generateBorderRadius
    ]



-- MODEL


init =
    { isTicking = True
    , time = Nothing
    , totalTicks = 0
    , dots = []
    , borderRadius = dotWidth / 2
    }
        ! generate



-- UPDATE


type Msg
    = ToggleIsTicking
    | Tick Time
    | SetDots (List Color)
    | SetBorderRadius Float


update msg model =
    case msg of
        ToggleIsTicking ->
            { model | isTicking = not model.isTicking } ! []

        Tick time ->
            { model
                | time = Just time
                , totalTicks = model.totalTicks + 1
            }
                ! generate

        SetDots dots ->
            { model | dots = dots } ! []

        SetBorderRadius borderRadius ->
            { model | borderRadius = borderRadius } ! []



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
        div [ style [ ( "backgroundColor", "black" ) ] ]
            [ div []
                [ div [ style [ ( "display", "flex" ), ( "flex-wrap", "wrap" ) ] ] <|
                    List.map (viewDot model.borderRadius) model.dots
                ]
            , button [ onClick ToggleIsTicking ]
                [ text
                    (if model.isTicking then
                        "Stop"
                     else
                        "Start"
                    )
                ]
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


toPx x =
    toString x ++ "px"


viewDot borderRadius color =
    let
        updateDotsIntervalInSeconds =
            Time.inSeconds updateDotsInterval

        viewStyle =
            style
                [ ( "backgroundColor", colorToString color )
                , ( "width", toPx dotWidth )
                , ( "height", toPx dotWidth )
                , ( "border-radius", toPx borderRadius )
                , ( "margin", "4px" )
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
