module Team exposing ( Type(..)
                     , Team
                     , Analysis
                     , analyse
                     , analyseHero)

import List
import Array exposing (Array)
import Hero exposing (Hero)

type Type = Ally | Enemy

type alias Team = Array Hero

type alias Analysis = { hero : Hero
                      , strongCounters : List Hero
                      , weakCounters : List Hero
                      }

analyse : Team -> Team -> Array Analysis
analyse allies enemies =
    Array.map (analyseHero enemies) allies

analyseHero : Team -> Hero -> Analysis
analyseHero enemies ally =
    let
        strong = List.filter (isCounter ally) (Array.toList enemies)
        weak = List.filter (isCounteredBy ally) (Array.toList enemies)
    in
        { hero = ally
        , strongCounters = strong
        , weakCounters = weak
        }

isCounter : Hero -> Hero -> Bool
isCounter subject target =
    (List.any (\weakness -> weakness == subject.name) target.weakCounters) ||
        (List.any (\strength -> strength == target.name) subject.strongCounters)

isCounteredBy : Hero -> Hero -> Bool
isCounteredBy subject target =
    (List.any (\strength -> strength == subject.name) target.strongCounters) ||
        (List.any (\weakness -> weakness == target.name) subject.weakCounters)
