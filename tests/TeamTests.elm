module TeamTests exposing (..)

import Test exposing (..)
import Expect
import Team
import Hero
import Array exposing (fromList)

all : Test
all =
    describe "Team"
        [ counters
        ]

heroA = { name = "a"
        , displayName = "A"
        , role = Hero.Support
        , strongCounters = []
        , weakCounters = ["c"]
        }

heroB = { name = "b"
        , displayName = "B"
        , role = Hero.Support
        , strongCounters = ["a"]
        , weakCounters = []
        }

heroC = { name = "c"
        , displayName = "C"
        , role = Hero.Support
        , strongCounters = []
        , weakCounters = ["b"]
        }

counters : Test
counters =
    describe "counters"
        [ test "returns heroes that contain the subject as a strongCounter" <|
              \() -> Team.counters heroB (fromList [heroA])
            |> Expect.equal [heroA]
        , test "returns heroes that are listed as weakCounters" <|
            \() -> Team.counters heroC (fromList [heroA])
                |> Expect.equal [heroA]
        , test "returns an empty list if no heroes are countered" <|
            \() -> Team.counters heroA (fromList [heroB, heroC])
                |> Expect.equal []
        ]

counteredBy : Test
counteredBy =
    describe "counteredBy"
        [ test "returns heroes that contain the subject as a weakCounter" <|
              \() -> Team.counteredBy heroC (fromList [heroA])
            |> Expect.equal [heroA]
        , test "returns heroes that are listed as strongCounters" <|
            \() -> Team.counteredBy heroA (fromList [heroB])
                |> Expect.equal [heroB]
        , test "returns an empty list list if there are no counters" <|
            \() -> Team.counteredBy heroB (fromList [heroA])
                |> Expect.equal []
        ]
