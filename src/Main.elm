import Html exposing (Html, div, text, input, span)
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
    { enemy = Array.fromList [ Hero.ana, Hero.ana, Hero.ana, Hero.ana, Hero.ana, Hero.ana ]
    , ally = Array.fromList [ Hero.ana, Hero.ana, Hero.ana, Hero.ana, Hero.ana, Hero.ana ]
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
              [ div [ class "enemy" ] (Array.toList (Array.indexedMap enemyHeroView model.enemy))
              , div [ class "ally" ] (Array.toList (Array.indexedMap allyHeroView model.ally))
              ]
        , div [ class (showHeroSelector model.changeHero) ] (heroListView Hero.heroes)
        ]

showHeroSelector : Maybe (Team.Type, Int) -> String
showHeroSelector m = case m of
                         Nothing -> "hidden selector"
                         Just _ -> "selector"

enemyHeroView i hero = span [ onClick (ChangeHero Team.Enemy i), class "hero" ] [ text hero.displayName ]
allyHeroView i hero = span [ onClick (ChangeHero Team.Ally i), class "hero" ] [ text hero.displayName ]

heroListView list = List.map (\hero -> span [ onClick (SetHero hero), class "hero" ] [ text hero.displayName ] ) list
