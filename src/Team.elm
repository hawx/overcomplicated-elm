module Team exposing ( Type(..)
                     , Team
                     , counters
                     , counteredBy
                     , bestCounter)

import List
import Array exposing (Array)
import Hero exposing (Hero)

type Type = Ally | Enemy

type alias Team = Array Hero

counters : Hero -> Team -> List Hero
counters hero enemies = List.filter (isCounter hero) (Array.toList enemies)

counteredBy : Hero -> Team -> List Hero
counteredBy hero enemies = List.filter (isCounteredBy hero) (Array.toList enemies)

bestCounter : Team -> Team -> Hero
bestCounter allies enemies =
    let
        allyList = Array.toList allies
        notAllies = List.filter (\h -> not (List.member h allyList)) Hero.heroes
        score = \hero -> (List.length (counteredBy hero enemies)) - (List.length (counters hero enemies))
        counterMatrix = List.map (\hero -> { hero = hero, score = score hero }) notAllies
        best = List.head (List.sortBy (.score) counterMatrix)
    in
        case best of
            Nothing -> Hero.hanzo
            Just x -> x.hero

isCounter : Hero -> Hero -> Bool
isCounter subject target =
    (List.any (\weakness -> weakness == subject.name) target.weakCounters) ||
        (List.any (\strength -> strength == target.name) subject.strongCounters)

isCounteredBy : Hero -> Hero -> Bool
isCounteredBy subject target =
    (List.any (\strength -> strength == subject.name) target.strongCounters) ||
        (List.any (\weakness -> weakness == target.name) subject.weakCounters)
