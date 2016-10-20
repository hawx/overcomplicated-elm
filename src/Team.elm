module Team exposing ( Type(..)
                     , Team
                     , counters
                     , counteredBy)

import List
import Array exposing (Array)
import Hero exposing (Hero)

type Type = Ally | Enemy

type alias Team = Array Hero

counters : Hero -> Team -> List Hero
counters hero enemies = List.filter (isCounter hero) (Array.toList enemies)

counteredBy : Hero -> Team -> List Hero
counteredBy hero enemies = List.filter (isCounteredBy hero) (Array.toList enemies)

isCounter : Hero -> Hero -> Bool
isCounter subject target =
    (List.any (\weakness -> weakness == subject.name) target.weakCounters) ||
        (List.any (\strength -> strength == target.name) subject.strongCounters)

isCounteredBy : Hero -> Hero -> Bool
isCounteredBy subject target =
    (List.any (\strength -> strength == subject.name) target.strongCounters) ||
        (List.any (\weakness -> weakness == target.name) subject.weakCounters)
