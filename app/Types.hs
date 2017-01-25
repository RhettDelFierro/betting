{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, LambdaCase, FlexibleInstances, NamedFieldPuns, DataKinds #-}

module Types where

type TeamName            = String
type Team_ID             = Int
type TeamABBR            = String
type Teams               = (TeamName,Team_ID,TeamABBR)
type WinningTeamBoxscore = GameResult
type LosingTeamBoxscore  = GameResult
type BoxScore            = (WinningTeamBoxscore,LosingTeamBoxscore)
type DayDiff             = Integer
type GameDate            = String

data WinningTeamStats = WinningTeamStats { pointDiff    :: Double
                                         , fieldGoalPct :: Double
                                         , rebounds     :: Double
                                         --, home         :: Bool
                                         , assists      :: Double
                                         , steals       :: Double
                                         , offReb       :: Double
                                         , turnovers    :: Double
                                         , threeFGA     :: Double
                                         , threeFGM     :: Double    --3p fg%
                                         , freeTAtt     :: Double    --freethrows attempted
                                         , freeTMade    :: Double    --freethrows made
                                         , blocks       :: Double
                                         , eFGPct       :: Double
                                         , home         :: Double
                                         , b2b          :: Double
                                         } deriving (Show, Eq, Ord, Read)

data GameResult = GameResult { team_ID   :: Int
                             , game_ID   :: String
                             , game_date :: String
                             , matchup   :: String
                             , wl        :: Char
                             , wi         :: Double --
                             , lo         :: Double --
                             , w_pct     :: Float
                             , minutes       :: Double --
                             , fgm       :: Double --
                             , fga       :: Double --
                             , fg_pct    :: Double
                             , fg3m      :: Double --
                             , fg3a      :: Double --
                             , fg3_pct   :: Float
                             , ftm       :: Double --
                             , fta       :: Double --
                             , ft_pct    :: Float
                             , oreb      :: Double --
                             , dreb      :: Double --
                             , reb       :: Double --
                             , ast       :: Double --
                             , stl       :: Double --
                             , blk       :: Double --
                             , tov       :: Double --
                             , pf        :: Double --
                             , pts       :: Double --
                             } deriving (Show, Eq, Read, Ord)

teams :: [Teams]
teams = [ ("Atlanta Hawks",1610612737,"ATL")
        , ("Boston Celtics",1610612738,"BOS")
        , ("Brooklyn Nets",1610612751,"BKN")
        , ("Charlotte Hornets",1610612766,"CHA")
        , ("Chicago Bulls",1610612741,"CHI")
        , ("Cleveland Cavaliers",1610612739,"CLE")
        , ("Dallas Mavericks",1610612742,"DAL")
        , ("Denver Nuggets",1610612743,"DEN")
        , ("Detroit Pistons",1610612765,"DET")
        , ("Golden State Warriors",1610612744,"GSW")
        , ("Houston Rockets",1610612745,"HOU")
        , ("Indiana Pacers",1610612754,"IND")
        , ("Los Angeles Clippers",1610612746,"LAC")
        , ("Los Angeles Lakers",1610612747,"LAL")
        , ("Memphis Grizzlies",1610612763,"MEM")
        , ("Miami Heat",1610612748,"MIA")
        , ("Milwaukee Bucks",1610612749,"MIL")
        , ("Minnesota Timberwolves",1610612750,"MIN")
        , ("New Orleans Pelicans",1610612740,"NOP")
        , ("New York Knicks",1610612752,"NYK")
        , ("Oklahoma City Thunder",1610612760,"OKC")
        , ("Orlando Magic",1610612753,"ORL")
        , ("Philadelphia 76ers",1610612755,"PHI")
        , ("Phoenix Suns",1610612756,"PHX")
        , ("Portland Trail Blazers",1610612757,"POR")
        , ("Sacramento Kings",1610612758,"SAC")
        , ("San Antonio Spurs",1610612759,"SAS")
        , ("Toronto Raptors",1610612761,"TOR")
        , ("Utah Jazz",1610612762,"UTA")
        , ("Washington Wizards",1610612764,"WAS")
        ]

data GameLogsByTeam = GameLogsByTeam [(Team_ID,[GameResult])] deriving (Show, Eq, Read, Ord)