{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Main where

import System.Environment

main :: IO ()
main = do
    api_key <- getEnv "NBA_API_KEY"
    putStrLn api_key

--all of these are differentials.
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
                                         } deriving (Show, Eq, Ord)



type TeamName  = String

type Team_ID   = Int
type Game_ID   = String
type Game_Date = String
type Matchup   = String
type WL        = Char
type Wins      = Int
type Losses    = Int
type W_Pct     = Float
type Min       = Int
type FGM       = Int
type FGA       = Int
type FG_Pct    = Float
type FG3M      = Int
type FG3A      = Int
type FG3_Pct   = Float
type FTM       = Int
type FTA       = Int
type FT_Pct    = Float
type OREB      = Int
type DREB      = Int
type REB       = Int
type AST       = Int
type STL       = Int
type BLK       = Int
type TOV       = Int
type PF        = Int
type PTS       = Int

type Teams    = (TeamName,Team_ID)

--will use this to get game logs.
teams :: [Teams]
teams = [("Atlanta Hawks", 1610612737)
        , ("Boston Celtics", 1610612738)
        , ("Brooklyn Nets",	1610612751)
        , ("Charlotte Hornets", 1610612766)
        , ("Chicago Bulls", 1610612741)
        , ("Cleveland Cavaliers", 1610612739)
        , ("Dallas Mavericks", 1610612742)
        , ("Denver Nuggets", 1610612743)
        , ("Detroit Pistons", 1610612765)
        , ("Golden State Warriors", 1610612744)
        , ("Houston Rockets", 1610612745)
        , ("Indiana Pacers", 1610612754)
        , ("Los Angeles Clippers", 1610612746)
        , ("Los Angeles Lakers", 1610612747)
        , ("Memphis Grizzlies",	1610612763)
        , ("Miami Heat", 1610612748)
        , ("Milwaukee Bucks", 1610612749)
        , ("Minnesota Timberwolves", 1610612750)
        , ("New Orleans Pelicans", 1610612740)
        , ("New York Knicks", 1610612752)
        , ("Oklahoma City Thunder",	1610612760)
        , ("Orlando Magic", 1610612753)
        , ("Philadelphia 76ers", 1610612755)
        , ("Phoenix Suns", 1610612756)
        , ("Portland Trail Blazers", 1610612757)
        , ("Sacramento Kings", 1610612758)
        , ("San Antonio Spurs", 1610612759)
        , ("Toronto Raptors", 1610612761)
        , ("Utah Jazz", 1610612762)
        , ("Washington Wizards", 1610612764)
        ]
data GameResult = GameResult {
                             , getTeam_ID :: Team_ID
                             , getGame_ID :: Game_ID
                             , getMatchup :: Matchup
                             , getWL      :: WL
                             , getWins    :: Wins
                             , getLosses  :: Losses
                             , getW_Pct   :: W_Pct
                             , getMin     :: Min
                             , getFGM     :: FGM
                             , getFGA     :: FGA
                             , getFG_Pct  :: FG_Pct
                             , getFG3M    :: FG3M
                             , getFG3A    :: FG3A
                             , getFG3_Pct :: FG3_Pct
                             , getFTM     :: FTM
                             , getFTA     :: FTA
                             , getFT_Pct  :: FT_Pct
                             , getOREB    :: OREB
                             , getDREB    :: DREB
                             , getREB     :: REB
                             , getAST     :: AST
                             , getSTL     :: STL
                             , getBLK     :: BLK
                             , getTOV     :: TOV
                             , getPF      :: PF
                             , getPTS     :: PTS
                             } deriving (Show, Eq, Ord)

getDiff :: GameResult -> GameResult -> WinngTeamStats
getDiff x y
    | checkGameID x y = case getWL x of
                          "W" -> winnerStats x y--I want to map a function to get the differences in stats from GameResult
                          "L" -> flip getDiff x y -- will call this function in inverse. BUT keep in mind you will do this for every game, so you may repeat.
                                --maybe drop both games from the list afterward. Compose the function maybe on a list.

checkGameID :: GameResult -> GameResult -> Bool
checkGameID x y = (==) (getGame_ID x) (getGame_ID y)

winnerStats :: GameResult -> GameResult -> WinngTeamStats
winnerStats x y = WinningTeamStats { pointDiff = (-) (getPTS x) (getPTS y)
                                   , fieldGoalPct = (-) (getFG_Pct x) (getFG_Pct y)
                                   , rebounds = (-) (getREB x) (getREB y)
                                   --, home     = get
                                   , assists = (-) (getAST x) (getAST y)
                                   , steals = (-) (getSTL x) (getSTL y)
                                   , offReb = (-) (getOREB x) (getOREB y)
                                   , turnovers = (-) (getTOV x) (getTOV y)
                                   , threeFGA = (-) (getFG3A x) (getFG3A y)
                                   , threeFGM = (-) (getFG3M x) (getFG3M y)
                                   , freeTAtt = (-) (getFTA x) (getFTA y)
                                   , freeTMade = (-) (getFTM x) (getFTM y)
                                   , fouls = (-) (getPF x) (getPF y)
                                   }
