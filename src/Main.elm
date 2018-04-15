module Main exposing (main)

import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Css
import Css.Colors
import Color exposing (Color)
import Time exposing (Time)
import Random exposing (Generator)
import Array exposing (Array)


-- CONFIG


dotCount : Int
dotCount =
    99


dotWidth : Float
dotWidth =
    150


updateDotsInterval : Time
updateDotsInterval =
    Time.second


colors : Array Color
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


randomColorGenerator : Generator Color
randomColorGenerator =
    Random.int 0 (Array.length colors - 1)
        |> Random.map
            (\index ->
                colors
                    |> Array.get index
                    |> Maybe.withDefault Color.white
            )


dotsGenerator : Generator (List Color)
dotsGenerator =
    Random.list dotCount randomColorGenerator


borderRadiusGenerator : Generator Float
borderRadiusGenerator =
    Random.float 0 (dotWidth / 2)



-- COMMANDS


generateDots : Cmd Msg
generateDots =
    Random.generate SetDots dotsGenerator


generateBorderRadius : Cmd Msg
generateBorderRadius =
    Random.generate SetBorderRadius borderRadiusGenerator


generate : List (Cmd Msg)
generate =
    [ generateDots
    , generateBorderRadius
    ]



-- MODEL


type alias Model =
    { isTicking : Bool
    , time : Maybe Time
    , totalTicks : Int
    , dots : List Color
    , borderRadius : Float
    }


init : ( Model, Cmd Msg )
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


update : Msg -> Model -> ( Model, Cmd Msg )
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


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isTicking then
        Time.every updateDotsInterval Tick
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        time =
            model.time
                |> Maybe.map toString
                |> Maybe.withDefault "N/A"
    in
        div [ css [ Css.backgroundColor Css.Colors.black ] ]
            [ div []
                [ div [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ] <|
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


viewDot : Float -> Color -> Html Msg
viewDot borderRadius color =
    let
        interval =
            Time.inSeconds updateDotsInterval

        { red, green, blue, alpha } =
            Color.toRgb color
    in
        div
            [ css
                [ Css.backgroundColor (Css.rgba red green blue alpha)
                , Css.width (Css.px dotWidth)
                , Css.height (Css.px dotWidth)
                , Css.borderRadius (Css.px borderRadius)
                , Css.margin (Css.px 4)
                ]
            , style [ ( "transition", "all " ++ toString interval ++ "s ease-out" ) ]
            ]
            []



-- PROGRAM


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view >> toUnstyled
        , subscriptions = subscriptions
        }
