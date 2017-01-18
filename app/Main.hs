{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import System.Environment

main :: IO ()
main = do
    api_key <- getEnv "NBA_API_KEY"
    putStrLn api_key

--all of these are differentials.
data WinningTeamStats = WinningTeamStats { pointDiff    :: Int
                                         , fieldGoalPct :: Double
                                         , rebounds     :: Int
                                         , home         :: Bool
                                         , assists      :: Int
                                         , steals       :: Int
                                         , offReb       :: Int
                                         , turnovers    :: Int
                                         , threeFGA     :: Int
                                         , threeFGM     :: Int    --3p fg%
                                         , freeTAtt     :: Int    --freethrows attempted
                                         , freeTMade    :: Int    --freethrows made
                                         , fouls        :: Int    --fouls commited
                                         , sFouls       :: Int    --shooting fouls
                                         } deriving (Show, Eq, Ord)



type TeamName = String
type TeamID   = Int
type Teams    = (TeamName,TeamID)

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