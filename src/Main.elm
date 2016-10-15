import Html exposing (Html, div, text, input, span, h1, ul, li)
import Html.Attributes exposing (class)
import Html.App
import Html.Events exposing (onInput, onClick)
import String
import List
import Debug
import Array exposing (Array)
import Hero exposing (Hero)
import Team exposing (Team)

main =
    Html.App.beginnerProgram { model = model, view = view, update = update }

-- Model

type alias Model =
    { enemy : Team
    , ally : Team
    , changeHero : Maybe (Team.Type, Int)
    }

model : Model
model =
    { enemy = Array.fromList [ Hero.mccree, Hero.roadhog, Hero.lucio, Hero.genji, Hero.reinhardt, Hero.zenyatta ]
    , ally = Array.fromList [ Hero.mccree, Hero.roadhog, Hero.lucio, Hero.genji, Hero.reinhardt, Hero.zenyatta ]
    , changeHero = Nothing
    }

-- Update

type Msg = ChangeHero Team.Type Int | SetHero Hero

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeHero team i ->
            { model | changeHero = Just (team, i) }

        SetHero hero ->
            case model.changeHero of
                Nothing -> { model | changeHero = Nothing }
                Just (teamType, index) -> case teamType of
                                              Team.Ally -> { model | changeHero = Nothing, ally = Array.set index hero model.ally }
                                              Team.Enemy -> { model | changeHero = Nothing, enemy = Array.set index hero model.enemy }

-- View

view : Model -> Html Msg
view model =
    div []
        [ div [ class "teams" ]
              [ h1 [ ] [ text "Enemy Team" ]
              , div [ class "enemy" ] <|
                    Array.toList (Array.indexedMap (heroView Team.Enemy (Array.fromList [])) model.enemy)
              , h1 [ ] [ text "Ally Team" ]
              , div [ class "ally" ] <|
                  Array.toList (Array.indexedMap (heroView Team.Ally model.enemy) model.ally)
              ]
        , div [ class (showHeroSelector model.changeHero) ] [ heroListView Hero.heroes ]
        ]

showHeroSelector : Maybe (Team.Type, Int) -> String
showHeroSelector m =
    case m of
        Nothing -> "hidden selector"
        Just _ -> "selector"

heroClass hero =
    "portrait " ++ hero.name ++ " " ++ toString hero.role

heroView teamType otherTeam i hero =
    div [ onClick (ChangeHero teamType i), class "hero" ]
        [ div [ class (heroClass hero) ] [ span [ class "name" ] [ text hero.displayName ] ]
        , strengthsView otherTeam hero ]

strengthsView otherTeam hero =
    let
        analysis = Team.analyseHero otherTeam hero
    in
        div [ ]
            [ ul [ class "strength" ] <|
                  List.map (\strong -> li [] [ text strong.displayName ]) analysis.strongCounters
            , ul [ class "weakness" ] <|
                List.map (\weak -> li [] [ text weak.displayName ]) analysis.weakCounters
            ]

heroListView list =
    div [ class "selector-inner" ] <| List.map (heroListItemView) list

heroListItemView hero =
    div [ onClick (SetHero hero), class ("hero small " ++ (heroClass hero)) ]
        [ span [ class "name" ] [ text hero.displayName ] ]
