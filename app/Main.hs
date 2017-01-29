{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveGeneric, RecordWildCards, LambdaCase, FlexibleInstances, NamedFieldPuns, DataKinds #-}

module Main where

import Data.Aeson
import qualified Data.Map as M
--import GHC.Exts
import qualified Data.ByteString.Lazy.Char8 as L
--import qualified Data.ByteString.Lazy as B
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Traversable as DT
import Data.Function (on)
import Data.Time
import Data.Time.Format
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (diffDays)
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.List as DL
import Types

import Debug.Trace


teamURLS :: [String]
teamURLS = fmap makeURL teams
    where makeURL = (\(_,team_id,_) -> "http://stats.nba.com/stats/teamgamelog/?Season=2016-17&SeasonType=Regular%20Season&TeamID=" ++ show team_id)

getJSON :: IO [B.ByteString]
getJSON = mapM simpleHttp teamURLS

main :: IO ()
main = do
  ds <- getJSON
  d <- return (eitherDecode <$> ds) :: IO [Either String (Maybe FullPage)]
  --putStrLn . show $ d
  let u  = fmap checkFullPage $ d
      v  = (map read u) :: [[GameResult]]
      w  = DL.concat v
      x  = removeOvertime w
      y  = getBoxScores x
      y' = getBoxScores w
      gamesWon = length $ filter ((=='W') . wl)  w
  --print z
  let results = winningTeamDefault { pointDiff    = (/) (sumInts pts y)   (fromIntegral $ length y)
                                   , fieldGoalPct = (/) (sumPct fg_pct y) (fromIntegral $ length y) * 100
                                   , rebounds     = (/) (sumInts reb y)   (fromIntegral $ length y)
                                   , assists      = (/) (sumInts ast y)   (fromIntegral $ length y)
                                   , steals       = (/) (sumInts stl y)   (fromIntegral $ length y)
                                   , offReb       = (/) (sumInts oreb y)  (fromIntegral $ length y)
                                   , turnovers    = (/) (sumInts tov y)   (fromIntegral $ length y)
                                   , threeFGA     = (/) (sumInts fg3a y)  (fromIntegral $ length y)
                                   , threeFGM     = (/) (sumInts fg3m y)  (fromIntegral $ length y)
                                   , freeTAtt     = (/) (sumInts fta y)   (fromIntegral $ length y)
                                   , freeTMade    = (/) (sumInts ftm y)   (fromIntegral $ length y)
                                   , blocks       = (/) (sumInts blk y)   (fromIntegral $ length y)
                                   , eFGPct       = (/) (effectiveFieldGoalPct y)            (fromIntegral $ length y)
                                   , home         = (/) (fromIntegral (length . snd . splitWinLossTeams . fst . splitHomeAwayTeams $ w)) (fromIntegral $ length y) * 100
                                   , b2b          = checkInterval w y' 1 1
                                   , threeInFour  = checkInterval w y' 4 2
                                   , fourInSix    = checkInterval w y' 6 3
                                   }
  print $ results

--checkInterval :: [GameResult] -> [(GameResult,GameResult)] -> DayDiff -> NumGamesPlayed -> [(Team_ID,Int)]
--checkInterval whole tup interval games_played = recordThroughDays $
--     gameDayInterval whole (map (\(x,_) -> (team_ID x,game_date x)) tup) interval games_played

checkInterval :: [GameResult] -> [(GameResult,GameResult)] -> DayDiff -> NumGamesPlayed -> [(TeamName,Wins,Losses)]
checkInterval whole tup interval games_played = stretchResults . recordThroughDays $ bothIntervals
    where bothIntervals = let winningInterval = gameDayInterval whole (map (\(x,_) -> (team_ID x,game_date x)) tup) interval games_played
                              losingInterval  = gameDayInterval whole (map (\(_,y) -> (team_ID y,game_date y)) tup) interval games_played
                          in  (winningInterval,losingInterval)
--     gameDayInterval whole (getOutcomes tup) interval games_played
--        where getOutcomes tup = let winning_interval =
--                                    losing_interval  =

stretchResults :: ([(TeamName,Int)],[(TeamName,Int)]) -> [(TeamName,Wins,Losses)]
stretchResults ts = [fullRecord x y | x <- (fst ts), y <- (snd ts), (fst x) == (fst y)]
            where fullRecord (t,u) (v,w) = (t,u,w)

gameDayInterval :: [GameResult] -> [(Team_ID,GameDate)] -> DayDiff -> NumGamesPlayed -> [[GameResult]]
gameDayInterval _ [] _  _ = []
gameDayInterval grs (x:xs) interval games_played = ((func1 . func2) $ grs) ++ (gameDayInterval grs xs interval games_played)
    where func1 = filter (((>= games_played) . length)) . DL.groupBy ((==) `on` (team_ID ))
          func2 = filter (\g -> ((team_ID g) == (fst x)) && (checkDates (snd x) interval (game_date g)))

checkDates :: GameDate -> DayDiff -> GameDate -> Bool
checkDates day1 dayDiff day2 = let d1 = parseTimeOrError False defaultTimeLocale "%b %d, %Y" day1 :: Day
                                   d2 = parseTimeOrError False defaultTimeLocale "%b %d, %Y" day2 :: Day
                               in ( (0 < (diffDays d1 d2)) && ((diffDays d1 d2) <= dayDiff) )

removeOvertime :: [GameResult] -> [GameResult]
removeOvertime = DL.filter (\x -> minutes x <= 240)

effectiveFieldGoalPct :: [(GameResult,GameResult)] -> Double
effectiveFieldGoalPct arr = sum $
    map (\(x,y) -> let a = ((fgm x) + (0.5 * (fg3m x))) / (fga x)
                       b = ((fgm y) + (0.5 * (fg3m y))) / (fga y)
                   in (a - b) * 100) arr

sumInts :: (GameResult -> Double) -> [(GameResult,GameResult)] -> Double
sumInts f arr = sum $ map (\(x,y) -> (f x) - (f y)) arr

sumPct :: (GameResult -> Double) -> [(GameResult,GameResult)] -> Double
sumPct f arr = sum $ map (\(x,y) -> (f x) - (f y)) arr



getBoxScores :: [GameResult] -> [(WinningTeamGameResult,LosingTeamGameResult)]
getBoxScores []     = []
getBoxScores (x:xs) =
    let sameGame = [boxscore a | a <- xs, (game_ID x) == (game_ID a) ]
    in getBoxScores xs ++ sameGame
        where boxscore t2
                 | (wl x) == 'W' = (x,t2)
                 | otherwise      = (t2,x)

splitHomeAwayTeams :: [GameResult] -> ([HomeTeamGameResult],[AwayTeamGameResult])
splitHomeAwayTeams = DL.partition ((elem '@') . matchup)

splitWinLossTeams :: [GameResult] -> ([WinningTeamGameResult],[LosingTeamGameResult])
splitWinLossTeams = DL.partition ((== 'W') . wl)

recordThroughDays :: ([[GameResult]],[[GameResult]]) -> ([(TeamName,Int)],[(TeamName,Int)])
recordThroughDays gr = let winning_teams = (initTeam teams) . (replaceTeamName teams) . frequency $ fmap (team_ID . head) $ fst gr
                           losing_teams  = (initTeam teams) . (replaceTeamName teams) . frequency $ fmap (team_ID . head) $ snd gr
                       in  (winning_teams,losing_teams)
--recordThroughDays :: [[GameResult]] -> [(Team_ID,Int)]
--recordThroughDays gr = frequency $ fmap (team_ID . head) gr

frequency :: [Team_ID] -> [(Team_ID, Int)]
frequency xs = M.toList (M.fromListWith (+) [(x, 1) | x <- xs])

replaceTeamName :: [Teams] -> [(Team_ID, Int)] -> [(TeamName, Int)]
replaceTeamName ts rs = [makeTeam x y | x@(t,u,v) <- ts, y <- rs, (u) == (fst y)]
                where makeTeam (t,u,v) b = (t,snd b)

initTeam :: [Teams] -> [(TeamName,Int)] -> [(TeamName,Int)]
initTeam ts rs = map (\x -> check x) ts
            where check (a,b,c)
                    | M.member a (M.fromList rs) = (a, M.findWithDefault 0 a (M.fromList rs))
                    | otherwise                   = (a,0)
--add b2b losses. Can use winsThroughDays and frequency to do that. I think all you need to change is the checkInterval to use (\(_,y)
--add team names to frequency/winsThroughDays

--to separate by records, just check the winning percentage of the other team. Unless you want tiers. But this will give you the team's current place in the league.
--maybe get the total record at the end of the season if you want a comparison. Just get the GameResults of teams



checkFullPage :: Either String (Maybe FullPage) -> String
checkFullPage = (\case Left err -> err
                       Right ds -> case ds of
                                      Just x -> show . showResult $ x
                                      _      -> "Maybe failed somewhere.")

showResult :: FullPage -> [GameResult]
showResult = (rowSet . DL.head . resultSets)

data ResultSets = ResultSets { rowSet :: [GameResult] }     deriving (Show, Eq, Read)
data FullPage   = FullPage   { resultSets :: [ResultSets] } deriving (Show, Eq, Read)

instance FromJSON FullPage where
  parseJSON = withObject "Result Sets" $ \o -> do
--      resource    <- o .: "resource"
--      parameters  <- o .: "parameters"
      resultSets  <- o .: "resultSets"
      return FullPage{..}

instance FromJSON ResultSets where
  parseJSON = withObject "Row Sets" $ \o -> do
--      name    <- o .: "name"
--      headers <- o .: "headers"
      rowSet  <- o .: "rowSet"
      return ResultSets{..}

instance FromJSON GameResult where
  --parseJSON arr = GameResults <$> (parseJSON arr)
  parseJSON arr = do
    [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1] <- parseJSON arr
    team_ID   <- parseJSON a
    game_ID   <- parseJSON b
    game_date <- parseJSON c
    matchup   <- parseJSON d
    wl        <- parseJSON e
    wi        <- parseJSON f
    lo        <- parseJSON g
    w_pct     <- parseJSON h
    minutes   <- parseJSON i
    fgm       <- parseJSON j
    fga       <- parseJSON k
    fg_pct    <- parseJSON l
    fg3m      <- parseJSON m
    fg3a      <- parseJSON n
    fg3_pct   <- parseJSON o
    ftm       <- parseJSON p
    fta       <- parseJSON q
    ft_pct    <- parseJSON r
    oreb      <- parseJSON s
    dreb      <- parseJSON t
    reb       <- parseJSON u
    ast       <- parseJSON v
    stl       <- parseJSON w
    blk       <- parseJSON x
    tov       <- parseJSON y
    pf        <- parseJSON z
    pts       <- parseJSON a1
    return GameResult{..}