module TeamTests exposing (..)

import Test exposing (..)
import Expect
import Team exposing (Team)
import Hero exposing (Hero)
import Array exposing (fromList)

all : Test
all =
    describe "Team"
        [ counters
        , counteredBy
        , uncountered
        , isCountered
        , score
        ]

heroA = Hero "a" "A" Hero.Support [] []
heroB = Hero "b" "B" Hero.Support [] []
heroC = Hero "c" "C" Hero.Support [] []

counterToA = Hero "cA" "CA" Hero.Support ["a"] []
counteredByA = Hero "dA" "DA" Hero.Support [] ["a"]

counterToB = Hero "cB" "CB" Hero.Support ["b"] []

counterToAB = Hero "cAB" "CAB" Hero.Support ["a", "b"] []
counteredByAB = Hero "dAB" "DAB" Hero.Support [] ["a", "b"]

counters : Test
counters =
    describe "counters"
        [ test "returns heroes that contain the subject as a strongCounter" <|
              \() -> Team.counters counterToA (fromList [heroA, heroC])
            |> Expect.equal [heroA]
        , test "returns heroes that are listed as weakCounters" <|
            \() -> Team.counters heroA (fromList [counteredByA, counteredByAB, heroC])
                |> Expect.equal [counteredByA, counteredByAB]
        , test "returns an empty list if no heroes are countered" <|
            \() -> Team.counters heroA (fromList [heroC])
                |> Expect.equal []
        ]

counteredBy : Test
counteredBy =
    describe "counteredBy"
        [ test "returns heroes that contain the subject as a weakCounter" <|
              \() -> Team.counteredBy heroA (fromList [counterToA, heroC])
            |> Expect.equal [counterToA]
        , test "returns heroes that are listed as strongCounters" <|
            \() -> Team.counteredBy counteredByA (fromList [heroA, counteredByAB, heroC])
                |> Expect.equal [heroA]
        , test "returns an empty list list if there are no counters" <|
            \() -> Team.counteredBy heroC (fromList [heroA])
                |> Expect.equal []
        ]

isCountered : Test
isCountered =
    describe "isCounteredByTeam"
        [ test "returns true if hero is countered by the team" <|
              \() -> Team.isCounteredByTeam (fromList [counterToA, heroC]) heroA
            |> Expect.true "isCounteredByTeam"
        , test "returns false if hero is not countered by the team" <|
            \() -> Team.isCounteredByTeam (fromList [heroB, heroC]) heroA
                |> Expect.false "not isCounteredByTeam"
        ]

uncountered : Test
uncountered =
    describe "uncountered"
        [ test "returns heroes who are not countered" <|
              \() -> Team.uncountered (fromList [counterToAB]) (fromList [heroC, heroA, heroB])
            |> Expect.equal [heroC]
        , test "returns an empty list if all heroes are countered" <|
            \() -> Team.uncountered (fromList [counterToAB]) (fromList [heroA, heroB])
                |> Expect.equal []
        ]

score : Test
score =
    describe "score"
        [ test "a counter is better than a non-counter" <|
              \() -> Team.score counterToA (fromList []) (fromList [heroA])
            |> Expect.lessThan (Team.score heroC (fromList []) (fromList [heroA]))
        , test "a countered hero is worse than a non-countered hero" <|
            \() -> Team.score counteredByA (fromList []) (fromList [heroA])
                |> Expect.greaterThan (Team.score heroC (fromList []) (fromList [heroA]))
        , test "a missing counter is better than a duplicate counter" <|
            \() -> Team.score counterToB (fromList [counterToA]) (fromList [heroA, heroB, heroC])
                |> Expect.lessThan (Team.score counterToA (fromList [counterToA]) (fromList [heroA, heroB, heroC]))
        ]
