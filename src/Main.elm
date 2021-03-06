module Main exposing (Hand(..), JudgeResult(..), Msg(..), Strategy(..), judgeHandWithStrategy, main, update)

import Browser
import Html exposing (Html, button, div, main_, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { strategy : Strategy, judgeResultMaybe : Maybe JudgeResult, lastHand : Hand }


type Hand
    = Guu
    | Choki
    | Paa


type Strategy
    = AlwaysHand Hand
    | SameHand
    | LastHand
    | LastWinHand


init : () -> ( Model, Cmd Msg )
init _ =
    ( { strategy = AlwaysHand Guu, judgeResultMaybe = Nothing, lastHand = Guu }, Cmd.none )



-- UPDATE


type Msg
    = SelectStrategy Strategy
    | DoJanken Hand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectStrategy strategy ->
            ( { model | strategy = strategy }, Cmd.none )

        DoJanken yourHand ->
            ( { model
                | judgeResultMaybe =
                    Just <|
                        judgeHandWithStrategy
                            { lastHand = model.lastHand
                            , strategy = model.strategy
                            , yourHand = yourHand
                            }
                , lastHand = yourHand
              }
            , Cmd.none
            )


type alias JudgeHandMaterail =
    { yourHand : Hand
    , lastHand : Hand
    , strategy : Strategy
    }


judgeHandWithStrategy : JudgeHandMaterail -> JudgeResult
judgeHandWithStrategy { lastHand, strategy, yourHand } =
    judgeHand yourHand
        (case strategy of
            AlwaysHand hand ->
                hand

            SameHand ->
                yourHand

            LastHand ->
                lastHand

            LastWinHand ->
                Maybe.withDefault Guu <|
                    List.head <|
                        List.filter (\cpuHand -> judgeHand cpuHand lastHand == Win)
                            [ Guu, Choki, Paa ]
        )


type JudgeResult
    = Win
    | Lose
    | Draw


judgeHand : Hand -> Hand -> JudgeResult
judgeHand yourHand cpuHand =
    case ( yourHand, cpuHand ) of
        ( Guu, Choki ) ->
            Win

        ( Guu, Paa ) ->
            Lose

        ( Choki, Paa ) ->
            Win

        ( Choki, Guu ) ->
            Lose

        ( Paa, Guu ) ->
            Win

        ( Paa, Choki ) ->
            Lose

        _ ->
            Draw



-- VIEW


view : Model -> Html Msg
view model =
    let
        unSelectedColor =
            "rgb(239, 239, 239)"

        selectedColor =
            "#D9E5FF"
    in
    main_ [ class "ly_cont" ]
        [ div []
            [ button [ onClick <| SelectStrategy <| AlwaysHand Guu ] [ text "????????????????????????" ]
            , case model.strategy of
                AlwaysHand hand ->
                    let
                        selectColor targetHand =
                            if targetHand == hand then
                                selectedColor

                            else
                                unSelectedColor
                    in
                    div []
                        [ button
                            [ style "background-color" <| selectColor Guu
                            , onClick <| SelectStrategy <| AlwaysHand Guu
                            ]
                            [ text "??????" ]
                        , button
                            [ style "background-color" <| selectColor Choki
                            , onClick <| SelectStrategy <| AlwaysHand Choki
                            ]
                            [ text "?????????" ]
                        , button
                            [ style "background-color" <| selectColor Paa
                            , onClick <| SelectStrategy <| AlwaysHand Paa
                            ]
                            [ text "??????" ]
                        ]

                _ ->
                    text ""
            , button
                [ onClick <| SelectStrategy SameHand
                , style "background-color" <|
                    case model.strategy of
                        SameHand ->
                            selectedColor

                        _ ->
                            unSelectedColor
                ]
                [ text "??????????????????????????????" ]
            , button
                [ onClick <| SelectStrategy LastHand
                , style "background-color" <|
                    case model.strategy of
                        LastHand ->
                            selectedColor

                        _ ->
                            unSelectedColor
                ]
                [ text "??????????????????????????????" ]
            , button
                [ onClick <| SelectStrategy LastWinHand
                , style "background-color" <|
                    case model.strategy of
                        LastWinHand ->
                            selectedColor

                        _ ->
                            unSelectedColor
                ]
                [ text "??????????????????????????????????????????" ]
            ]
        , div []
            [ p [] [ text "?????????????????????" ]
            , div []
                [ button
                    [ onClick <| DoJanken Guu
                    ]
                    [ text "??????" ]
                , button
                    [ onClick <| DoJanken Choki
                    ]
                    [ text "?????????" ]
                , button
                    [ onClick <| DoJanken Paa
                    ]
                    [ text "??????" ]
                ]
            ]
        , div []
            [ p [] [ text "????????????" ]
            , case model.judgeResultMaybe of
                Just judgeResult ->
                    text <|
                        case judgeResult of
                            Win ->
                                "??????"

                            Lose ->
                                "??????"

                            Draw ->
                                "????????????"

                Nothing ->
                    text ""
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
