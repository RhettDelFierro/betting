{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, LambdaCase, FlexibleInstances, NamedFieldPuns, DataKinds #-}

module Types where

type TeamName  = String
type Team_ID   = Int
type TeamABBR  = String
type Teams    = (TeamName,Team_ID, TeamABBR)

data WinningTeamStats = WinningTeamStats { pointDiff    :: Int
                                         , fieldGoalPct :: Float
                                         , rebounds     :: Int
                                         --, home         :: Bool
                                         , assists      :: Int
                                         , steals       :: Int
                                         , offReb       :: Int
                                         , turnovers    :: Int
                                         , threeFGA     :: Int
                                         , threeFGM     :: Int    --3p fg%
                                         , freeTAtt     :: Int    --freethrows attempted
                                         , freeTMade    :: Int    --freethrows made
                                         , fouls        :: Int    --fouls commited
                                         } deriving (Show, Eq, Ord, Read)

data GameResult = GameResult { team_ID   :: Int
                             , game_ID   :: String
                             , game_date :: String
                             , matchup   :: String
                             , wl        :: Char
                             , wi         :: Int
                             , lo         :: Int
                             , w_pct     :: Float
                             , min       :: Int
                             , fgm       :: Int
                             , fga       :: Int
                             , fg_pct    :: Float
                             , fg3m      :: Int
                             , fg3a      :: Int
                             , fg3_pct   :: Float
                             , ftm       :: Int
                             , fta       :: Int
                             , ft_pct    :: Float
                             , oreb      :: Int
                             , dreb      :: Int
                             , reb       :: Int
                             , ast       :: Int
                             , stl       :: Int
                             , blk       :: Int
                             , tov       :: Int
                             , pf        :: Int
                             , pts       :: Int
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

data GameLogs = GameLogs [(Team_ID,[GameResult])] deriving (Show, Eq, Read, Ord)