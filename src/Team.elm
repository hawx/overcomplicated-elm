module Team exposing (..)

import Array exposing (Array)
import Hero exposing (Hero)

type Type = Ally | Enemy

type alias Team = Array Hero
