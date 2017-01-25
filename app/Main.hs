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
import Data.TimeCalendar (diffDays)
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.List as DL
import Types


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
  let u = fmap checkFullPage $ d
      v = (map read u) :: [[GameResult]]
      w = DL.concat v --::[GameResult]
      x = removeOvertime w --::[GameResult] with no overtime
      y = getBoxScores x -- [(GameResult,GameResult)] with no overtime
  --print z
  print $ WinningTeamStats { pointDiff    = (/) (sumInts pts y)   (fromIntegral $ length y)
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
                           , home         = (/) (fromIntegral (checkHome matchup y)) (fromIntegral $ length y) * 100
                           , b2b          = (/) (checkB2B w y)(fromIntegral $ length y)
                           }

checkB2B :: [GameResult] -> [(GameResult,GameResult)] -> Double
checkB2B whole tup = length $

checkDates :: String -> Integer -> String -> Bool
checkDates day1 dayDiff day2 = let d1     = parseTimeOrError False defaultTimeLocale "%b %d, %Y" day1 :: Day
                                   d2     = parseTimeOrError False defaultTimeLocale "%b %d, %Y" day2 :: Day
                                in (diffDays d1 d2 <= dayDiff)

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

checkFullPage :: Either String (Maybe FullPage) -> String
checkFullPage = (\case Left err -> err
                       Right ds -> case ds of
                                      Just x -> show . showResult $ x
                                      _      -> "Maybe failed somewhere.")

showResult :: FullPage -> [GameResult]
showResult = (rowSet . DL.head . resultSets)

getBoxScores :: [GameResult] -> [(GameResult,GameResult)]
getBoxScores []     = []
getBoxScores (x:xs) =
    let sameGame = [boxscore a | a <- xs, (game_ID x) == (game_ID a) ]
    in getBoxScores xs ++ sameGame
        where boxscore t2
                 | (wl x) == 'W' = (x,t2)
                 | otherwise      = (t2,x)

checkHome :: (GameResult -> String) -> [(GameResult,GameResult)] -> Int
checkHome f arr = length $ filter (\(x,_) -> elem '@' $ f x) arr








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




--will use this to get game logs.


