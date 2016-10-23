module Team exposing ( Type(..)
                     , Team
                     , displayName
                     , counters
                     , counteredBy
                     , uncountered
                     , isUncounteredByTeam
                     , isCounteredByTeam
                     , score
                     , bestCounter)

import Debug
import List
import Array exposing (Array)
import Hero exposing (Hero)

type Type = Ally | Enemy

displayName t = case t of
                    Ally -> "Ally Team"
                    Enemy -> "Enemy Team"

type alias Team = Array Hero

counters : Hero -> Team -> List Hero
counters hero enemies = List.filter (isCounter hero) (Array.toList enemies)

counteredBy : Hero -> Team -> List Hero
counteredBy hero enemies = List.filter (isCounteredBy hero) (Array.toList enemies)

uncountered : Team -> Team -> List Hero
uncountered allies enemies = List.filter (isUncounteredByTeam allies) (Array.toList enemies)

bestCounter : Team -> Team -> Hero
bestCounter allies enemies =
    let
        allyList = Array.toList allies
        notAllies = List.filter (\h -> not (List.member h allyList)) Hero.heroes
        counterMatrix = List.map (\hero -> { hero = hero, score = score hero allies enemies }) notAllies
        best = List.head (List.sortBy (.score) counterMatrix)
    in
        case best of
            Nothing -> Hero.hanzo
            Just x -> x.hero

score : Hero -> Team -> Team -> Int
score hero allies enemies =
    let
        strengths = List.length (hero `counters` enemies)
        weaknesses = List.length (hero `counteredBy` enemies)
        isMissingCounter = if List.any (isCounter hero) (uncountered allies enemies) then 1 else 0
    in
        weaknesses - strengths - isMissingCounter

isCounter : Hero -> Hero -> Bool
isCounter subject target = subject `isWeak` target || target `isStrong` subject

isCounteredBy : Hero -> Hero -> Bool
isCounteredBy subject target = target `isCounter` subject

isCounteredByTeam : Team -> Hero -> Bool
isCounteredByTeam team subject = List.any (isCounteredBy subject) (Array.toList team)

isUncounteredByTeam : Team -> Hero -> Bool
isUncounteredByTeam team = not << isCounteredByTeam team

isStrong : Hero -> Hero -> Bool
isStrong a b = List.any (\strength -> strength == a.name) b.strongCounters

isWeak : Hero -> Hero -> Bool
isWeak a b = List.any (\weakness -> weakness == a.name) b.weakCounters
