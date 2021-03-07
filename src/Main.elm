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
            [ button [ onClick <| SelectStrategy <| AlwaysHand Guu ] [ text "常に同じ手を出す" ]
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
                            [ text "グー" ]
                        , button
                            [ style "background-color" <| selectColor Choki
                            , onClick <| SelectStrategy <| AlwaysHand Choki
                            ]
                            [ text "チョキ" ]
                        , button
                            [ style "background-color" <| selectColor Paa
                            , onClick <| SelectStrategy <| AlwaysHand Paa
                            ]
                            [ text "パー" ]
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
                [ text "あなたと同じ手を出す" ]
            , button
                [ onClick <| SelectStrategy LastHand
                , style "background-color" <|
                    case model.strategy of
                        LastHand ->
                            selectedColor

                        _ ->
                            unSelectedColor
                ]
                [ text "最後に出した手を出す" ]
            , button
                [ onClick <| SelectStrategy LastWinHand
                , style "background-color" <|
                    case model.strategy of
                        LastWinHand ->
                            selectedColor

                        _ ->
                            unSelectedColor
                ]
                [ text "最後に出した手に勝つ手を出す" ]
            ]
        , div []
            [ p [] [ text "あなたの手は？" ]
            , div []
                [ button
                    [ onClick <| DoJanken Guu
                    ]
                    [ text "グー" ]
                , button
                    [ onClick <| DoJanken Choki
                    ]
                    [ text "チョキ" ]
                , button
                    [ onClick <| DoJanken Paa
                    ]
                    [ text "パー" ]
                ]
            ]
        , div []
            [ p [] [ text "勝敗は？" ]
            , case model.judgeResultMaybe of
                Just judgeResult ->
                    text <|
                        case judgeResult of
                            Win ->
                                "勝ち"

                            Lose ->
                                "負け"

                            Draw ->
                                "引き分け"

                Nothing ->
                    text ""
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
