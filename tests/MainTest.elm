module MainTest exposing (..)

import Expect
import Fuzz exposing (string)
import Main exposing (Hand(..), JudgeResult(..), Msg(..), Strategy(..), judgeHandWithStrategy, update)
import Test exposing (..)


judgeHandWithStrategyTest : Test
judgeHandWithStrategyTest =
    describe "#judgeHandWithStrategy"
        [ describe "「常にチョキを出す戦略の時」グーを出せば"
            [ test "勝つ" <|
                \_ ->
                    judgeHandWithStrategy { lastHand = Guu, yourHand = Guu, strategy = AlwaysHand Choki }
                        |> Expect.equal Win
            ]
        , describe "「常にチョキを出す戦略の時」、パーを出せば"
            [ test "負ける" <|
                \_ ->
                    judgeHandWithStrategy { lastHand = Guu, yourHand = Paa, strategy = AlwaysHand Choki }
                        |> Expect.equal Lose
            ]
        , describe "「常に自分と同じを出す戦略の時」、グーを出せば"
            [ test "引き分けになる" <|
                \_ ->
                    judgeHandWithStrategy { lastHand = Guu, yourHand = Guu, strategy = SameHand }
                        |> Expect.equal Draw
            ]
        , describe "「常に自分と同じを出す戦略の時」、チョキを出せば"
            [ test "引き分けになる" <|
                \_ ->
                    judgeHandWithStrategy { lastHand = Guu, yourHand = Choki, strategy = SameHand }
                        |> Expect.equal Draw
            ]
        , describe "「常に自分と同じを出す戦略の時」、パーを出せば"
            [ test "引き分けになる" <|
                \_ ->
                    judgeHandWithStrategy { lastHand = Guu, yourHand = Paa, strategy = SameHand }
                        |> Expect.equal Draw
            ]
        , describe "「最後に出した手を出す戦略の時」、最後の手がグーであるならば、パーを出せば"
            [ test "勝つ" <|
                \_ ->
                    judgeHandWithStrategy { lastHand = Guu, yourHand = Paa, strategy = LastHand }
                        |> Expect.equal Win
            ]
        , describe "「最後に出した手を出す戦略の時」、最後の手がチョキであるならば、パーを出せば"
            [ test "負ける" <|
                \_ ->
                    judgeHandWithStrategy { lastHand = Choki, yourHand = Paa, strategy = LastHand }
                        |> Expect.equal Lose
            ]
        , describe "「最後に出した手に勝つ手を出す戦略の時」、最後の手がチョキであるならば、パーを出せば"
            [ test "勝つ" <|
                \_ ->
                    judgeHandWithStrategy { lastHand = Choki, yourHand = Paa, strategy = LastWinHand }
                        |> Expect.equal Win
            ]
        , describe "「最後に出した手に勝つ手を出す戦略の時」、最後の手がグーであるならば、パーを出せば"
            [ test "引き分けになる" <|
                \_ ->
                    judgeHandWithStrategy { lastHand = Guu, yourHand = Paa, strategy = LastWinHand }
                        |> Expect.equal Draw
            ]
        ]
