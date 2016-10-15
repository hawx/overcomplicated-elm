module Hero exposing (..)

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

genji = { name = "genji"
        , displayName = "Genji"
        , role = Offense
        , strongCounters = [ "bastion", "mercy", "torbjorn" ]
        , weakCounters = [ "mei", "zarya", "winston" ]
        }

hanzo = { name = "hanzo"
        , displayName = "Hanzo"
        , role = Defense
        , strongCounters = [ "bastion", "torbjorn", "widowmaker" ]
        , weakCounters = [ "dva", "tracer", "widowmaker" ]
        }

junkrat = { name = "junkrat"
          , displayName = "Junkrat"
          , role = Defense
          , strongCounters = [ "bastion", "mei", "torbjorn" ]
          , weakCounters = [ "mccree", "pharah", "widowmaker" ]
          }

lucio = { name = "lucio"
        , displayName = "Lucio"
        , role = Support
        , strongCounters = [ "dva", "reaper", "winston" ]
        , weakCounters = [ "mei", "mccree", "pharah" ]
        }

mccree = { name = "mccree"
         , displayName = "McCree"
         , role = Offense
         , strongCounters = [ "reaper", "tracer", "winston" ]
         , weakCounters = [ "genji", "soldier76", "widowmaker" ]
         }

mei = { name = "mei"
      , displayName = "Mei"
      , role = Defense
      , strongCounters = [ "genji", "winston", "tracer" ]
      , weakCounters = [ "junkrat", "pharah", "widowmaker" ]
      }

mercy = { name = "mercy"
        , displayName = "Mercy"
        , role = Support
        , strongCounters = []
        , weakCounters = [ "mccree", "tracer", "widowmaker" ]
        }

pharah = { name = "pharah"
         , displayName = "Pharah"
         , role = Offense
         , strongCounters = [ "bastion", "junkrat", "mei" ]
         , weakCounters = [ "mccree", "roadhog", "soldier76" ]
         }

reaper = { name = "reaper"
         , displayName = "Reaper"
         , role = Offense
         , strongCounters = [ "bastion", "mei", "winston" ]
         , weakCounters = [ "junkrat", "mccree", "pharah" ]
         }

reinhardt = { name = "reinhardt"
            , displayName = "Reinhardt"
            , role = Tank
            , strongCounters = [ "soldier76", "torbjorn", "widowmaker" ]
            , weakCounters = [ "reaper", "roadhog", "symmetra" ]
            }

roadhog = { name = "roadhog"
          , displayName = "Roadhog"
          , role = Tank
          , strongCounters = [ "pharah", "reinhardt", "tracer" ]
          , weakCounters = [ "dva", "genji", "reaper" ]
          }

soldier76 = { name = "soldier76"
            , displayName = "Soldier: 76"
            , role = Offense
            , strongCounters = [ "mercy", "pharah", "torbjorn" ]
            , weakCounters = [ "genji", "mei", "tracer" ]
            }

symmetra = { name = "symmetra"
           , displayName = "Symmetra"
           , role = Support
           , strongCounters = [ "bastion", "dva", "reinhardt" ]
           , weakCounters = [ "junkrat", "pharah", "roadhog" ]
           }

torbjorn = { name = "torbjorn"
           , displayName = "Torbjörn"
           , role = Defense
           , strongCounters = [ "genji", "lucio", "tracer" ]
           , weakCounters = [ "junkrat", "pharah", "widowmaker" ]
           }

tracer = { name = "tracer"
         , displayName = "Tracer"
         , role = Offense
         , strongCounters = [ "bastion", "mercy", "widowmaker" ]
         , weakCounters = [ "mccree", "mei", "soldier76" ]
         }

widowmaker = { name = "widowmaker"
             , displayName = "Widowmaker"
             , role = Defense
             , strongCounters = [ "bastion", "torbjorn", "pharah" ]
             , weakCounters = [ "dva", "genji", "winston" ]
             }

winston = { name = "winston"
          , displayName = "Winston"
          , role = Tank
          , strongCounters = [ "genji", "hanzo", "widowmaker" ]
          , weakCounters = [ "mccree", "mei", "reaper" ]
          }

zarya = { name = "zarya"
        , displayName = "Zarya"
        , role = Tank
        , strongCounters = [ "dva", "genji", "winston" ]
        , weakCounters = [ "pharah", "roadhog", "reaper" ]
        }

zenyatta = { name = "zenyatta"
           , displayName = "Zenyatta"
           , role = Support
           , strongCounters = [ "dva", "roadhog", "winston" ]
           , weakCounters = [ "hanzo", "tracer", "widowmaker" ]
           }

heroes : List Hero
heroes = [ ana, bastion, dva, genji, hanzo, junkrat, lucio, mccree, mei, mercy, pharah, reaper, reinhardt, roadhog, soldier76, symmetra, torbjorn, tracer, widowmaker, winston, zarya, zenyatta ]
