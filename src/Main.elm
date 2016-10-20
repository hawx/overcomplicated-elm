import Html exposing (Html, div, text, input, span, h1, ul, li)
import Html.Attributes exposing (class)
import Html.App
import Html.Events exposing (onInput, onClick)
import String
import List
import Keyboard
import Debug
import Array exposing (Array)
import Hero exposing (Hero)
import Team exposing (Team)

main =
    Html.App.program { init = init, view = view, update = update, subscriptions = subscriptions }

-- Model

bestCounter : Model -> Hero
bestCounter model = Team.bestCounter model.enemy

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

type Msg = ChangeHero Team.Type Int | SetHero Team.Type Int Hero | KeyPress Keyboard.KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeHero team i ->
            ({ model | changeHero = Just { team = team, i = i } }, Cmd.none)

        SetHero team index hero ->
            case team of
                Team.Ally -> ({ model | changeHero = Nothing, ally = Array.set index hero model.ally }, Cmd.none)
                Team.Enemy -> ({ model | changeHero = Nothing, enemy = Array.set index hero model.enemy }, Cmd.none)

        KeyPress code ->
            if code == 27 then
                ({ model | changeHero = Nothing }, Cmd.none)
            else
                (model, Cmd.none)

-- View

view : Model -> Html Msg
view model =
    div [] <|
        [ div [ class "teams" ]
              [ h1 [ ] [ text "Enemy Team" ]
              , div [ class "enemy" ] <|
                    Array.toList (Array.indexedMap (heroView Team.Enemy (Array.fromList [])) model.enemy)
              , h1 [ ] [ text "Ally Team" ]
              , div [ class "ally" ] <|
                  Array.toList (Array.indexedMap (heroView Team.Ally model.enemy) model.ally)
              ]
        ] ++ ( heroListView Hero.heroes model.changeHero )

showHeroSelector : Maybe HeroChange -> String
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
        strengths = Team.counters hero otherTeam
        weaknesses = Team.counteredBy hero otherTeam
    in
        div [ ]
            [ ul [ class "strength" ] <|
                  List.map (\strong -> li [] [ text strong.displayName ]) strengths
            , ul [ class "weakness" ] <|
                List.map (\weak -> li [] [ text weak.displayName ]) weaknesses
            ]

heroListView : List Hero -> Maybe HeroChange -> List (Html Msg)
heroListView list change =
    case change of
        Nothing -> []
        Just c -> [ div [ class "selector" ]
                        [ div [ class "selector-inner" ] <| List.map (heroListItemView c.team c.i) list ]
                  ]

heroListItemView team index hero =
    div [ onClick (SetHero team index hero), class ("hero small " ++ (heroClass hero)) ]
        [ span [ class "name" ] [ text hero.displayName ] ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyPress
