import Html exposing (Html, div, text, input, span)
import Html.Attributes exposing (class)
import Html.App
import Html.Events exposing (onInput, onClick)
import String
import List
import Debug
import Array exposing (Array)

main =
    Html.App.beginnerProgram { model = model, view = view, update = update }

-- Model

type TeamType = Ally | Enemy

type Role = Support | Tank | Defense | Offense

type alias Hero = { name : String
                  , displayName : String
                  , role : Role
                  , strongCounters : List String
                  , weakCounters : List String
                  }

ana : Hero
ana = { name = "ana"
      , displayName = "Ana"
      , role = Support
      , strongCounters = [ "lucio", "mercy", "zenyatta" ]
      , weakCounters = [ "genji", "reaper", "tracer" ]
      }

bastion : Hero
bastion = { name = "bastion"
          , displayName = "Bastion"
          , role = Defense
          , strongCounters = [ "mercy", "reinhardt", "winston" ]
          , weakCounters = [ "genji", "tracer", "widowmaker" ]
          }

dva : Hero
dva = { name = "dva"
      , displayName = "D.Va"
      , role = Tank
      , strongCounters = [ "pharah", "reinhardt", "windowmaker" ]
      , weakCounters = [ "mei", "zarya", "winston" ]
      }

heroes : List Hero
heroes = [ ana, bastion, dva ]

type alias Team = Array Hero

type alias Model =
    { enemy : Team
    , ally : Team
    , changeHero : Maybe (TeamType, Int)
    }

model : Model
model =
    { enemy = Array.fromList [ ana, ana, ana, ana, ana, ana ]
    , ally = Array.fromList [ ana, ana, ana, ana, ana, ana ]
    , changeHero = Nothing
    }

-- Update

type Msg = ChangeHero TeamType Int | SetHero Hero

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeHero team i ->
            { model | changeHero = Just (team, i) }

        SetHero hero ->
            case model.changeHero of
                Nothing -> { model | changeHero = Nothing }
                Just (teamType, index) -> case teamType of
                                              Ally -> { model | changeHero = Nothing, ally = Array.set index hero model.ally }
                                              Enemy -> { model | changeHero = Nothing, enemy = Array.set index hero model.enemy }

-- View

view : Model -> Html Msg
view model =
    div []
        [ div [ class "teams" ]
              [ div [ class "enemy" ] (Array.toList (Array.indexedMap enemyHeroView model.enemy))
              , div [ class "ally" ] (Array.toList (Array.indexedMap allyHeroView model.ally))
              ]
        , div [ class (showHeroSelector model.changeHero) ] (heroListView heroes)
        ]

showHeroSelector : Maybe (TeamType, Int) -> String
showHeroSelector m = case m of
                         Nothing -> "hidden selector"
                         Just _ -> "selector"

enemyHeroView i hero = span [ onClick (ChangeHero Enemy i), class "hero" ] [ text hero.displayName ]
allyHeroView i hero = span [ onClick (ChangeHero Ally i), class "hero" ] [ text hero.displayName ]

heroListView list = List.map (\hero -> span [ onClick (SetHero hero), class "hero" ] [ text hero.displayName ] ) list
