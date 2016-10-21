import Html exposing (Html, div, text, input, span, h1, ul, li)
import Html.Attributes exposing (class)
import Html.App
import Html.Events exposing (onInput, onClick)
import String
import List
import Keyboard
import Debug
import Array exposing (Array)
import Array.Extra
import Hero exposing (Hero)
import Team exposing (Team)

main =
    Html.App.program { init = init, view = view, update = update, subscriptions = subscriptions }

-- Model

type alias HeroChange = { team : Team.Type, i : Int }

type alias Model =
    { enemy : Team
    , ally : Team
    , changeHero : Maybe HeroChange
    }

model : Model
model =
    { enemy = Array.fromList [ Hero.mccree, Hero.roadhog, Hero.lucio, Hero.genji, Hero.reinhardt, Hero.zenyatta ]
    , ally = Array.fromList [ Hero.mccree, Hero.roadhog, Hero.lucio, Hero.genji, Hero.reinhardt, Hero.zenyatta ]
    , changeHero = Nothing
    }

init : (Model, Cmd Msg)
init = (model, Cmd.none)

-- Update

type Msg = ChangeHero Team.Type Int | SetHero Team.Type Int Hero | SetAuto Int | KeyPress Keyboard.KeyCode

updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        ChangeHero team i ->
            { model | changeHero = Just { team = team, i = i } }

        SetHero team index hero ->
            case team of
                Team.Ally -> { model | changeHero = Nothing, ally = Array.set index hero model.ally }
                Team.Enemy -> { model | changeHero = Nothing, enemy = Array.set index hero model.enemy }

        SetAuto index ->
            { model | changeHero = Nothing, ally = Array.set index (Team.bestCounter (Array.Extra.removeAt index model.ally) model.enemy) model.ally }

        KeyPress code ->
            if code == 27 then
                { model | changeHero = Nothing }
            else
                model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (updateModel msg model, Cmd.none)

-- View

view : Model -> Html Msg
view model =
    let
        enemyTeam = teamView Team.Enemy (Array.fromList []) model.enemy
        allyTeam = teamView Team.Ally model.enemy model.ally
        selector = selectorView Hero.heroes model.enemy model.changeHero
    in
        div [] <|
            [ div [ class "teams" ] (enemyTeam ++ allyTeam) ] ++ selector

teamView teamType enemy ally =
    [ h1 [ ] [ text (Team.displayName teamType) ]
    , div [ class "team" ] <|
        Array.toList (Array.indexedMap (heroView teamType enemy) ally)
    ]

heroClass hero =
    "portrait " ++ hero.name ++ " " ++ toString hero.role

heroView teamType otherTeam i hero =
    div [ onClick (ChangeHero teamType i), class "hero" ]
        [ div [ class (heroClass hero) ] [ span [ class "name" ] [ text hero.displayName ] ]
        , strengthsView otherTeam hero ]

strengthsView otherTeam hero =
    let
        strengths = Team.counters hero otherTeam
        weaknesses = Team.counteredBy hero otherTeam
        list = List.map (\item -> li [] [ text item.displayName ])
    in
        div [ ]
            [ ul [ class "strength" ] (list strengths)
            , ul [ class "weakness" ] (list weaknesses)
            ]

selectorView : List Hero -> Team -> Maybe HeroChange -> List (Html Msg)
selectorView list enemies change =
    case change of
        Nothing -> []
        Just c ->
            let
                heroes = List.map (selectorItemView c.team c.i) list
                inner = case c.team of
                            Team.Ally -> [ div [ class "selector-inner" ] (heroes ++ [bestCounterItemView c.i]) ]
                            Team.Enemy -> [ div [ class "selector-inner" ] heroes ]
            in
                [ div [ class "selector" ] inner ]

selectorItemView team index hero =
    div [ onClick (SetHero team index hero), class ("hero small " ++ (heroClass hero)) ]
        [ span [ class "name" ] [ text hero.displayName ] ]

bestCounterItemView index =
    div [ onClick (SetAuto index), class ("hero small auto") ]
        [ span [ class "name" ] [ text "Best Counter" ] ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyPress
