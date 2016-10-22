module Tests exposing (..)

import Test exposing (..)
import TeamTests

all : Test
all =
    describe "Overcomplicated"
        [ TeamTests.all
        ]
