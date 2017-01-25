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
  let x = fmap checkFullPage $ d
      y = (map read x) :: [[GameResult]]
      z = getBoxScores . removeOvertime . DL.concat $ y
  --print z
  print $ WinningTeamStats { pointDiff = (fromIntegral (sumInts pts z)) / (fromIntegral $ length z)
                           , fieldGoalPct = ((sumPct fg_pct z) * 100) / (fromIntegral $ length z)
                           , rebounds = (fromIntegral (sumInts reb z)) / (fromIntegral $ length z)
                           , assists = (fromIntegral (sumInts ast z)) / (fromIntegral $ length z)
                           , steals = (fromIntegral (sumInts stl z)) / (fromIntegral $ length z)
                           , offReb = (fromIntegral (sumInts oreb z)) / (fromIntegral $ length z)
                           , turnovers = (fromIntegral (sumInts tov z)) / (fromIntegral $ length z)
                           , threeFGA = (fromIntegral (sumInts fg3a z)) / (fromIntegral $ length z)
                           , threeFGM = (fromIntegral (sumInts fg3m z)) / (fromIntegral $ length z)
                           , freeTAtt = (fromIntegral (sumInts fta z)) / (fromIntegral $ length z)
                           , freeTMade = (fromIntegral (sumInts ftm z)) / (fromIntegral $ length z)
                           , blocks = (fromIntegral (sumInts blk z)) / (fromIntegral $ length z)
                           , eFGPct = (effectiveFieldGoalPct z) / (fromIntegral $ length z)
                           }

removeOvertime :: [GameResult] -> [GameResult]
removeOvertime = DL.filter (\x -> minutes x <= 240)

effectiveFieldGoalPct :: [(GameResult,GameResult)] -> Double
effectiveFieldGoalPct arr = sum $
    map (\(x,y) -> ((fromIntegral (fgm x)) - (fromIntegral (fgm y)))
            + 0.5 * ((fromIntegral (fg3m x)) - (fromIntegral (fg3m y)))
                / ((fromIntegral (fga x)) - (fromIntegral (fga y)))) arr

sumInts :: (GameResult -> Int) -> [(GameResult,GameResult)] -> Int
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
    team_ID <- parseJSON a
    game_ID <- parseJSON b
    game_date <- parseJSON c
    matchup <- parseJSON d
    wl <- parseJSON e
    wi <- parseJSON f
    lo <- parseJSON g
    w_pct <- parseJSON h
    minutes <- parseJSON i
    fgm <- parseJSON j
    fga <- parseJSON k
    fg_pct <- parseJSON l
    fg3m <- parseJSON m
    fg3a <- parseJSON n
    fg3_pct <- parseJSON o
    ftm <- parseJSON p
    fta <- parseJSON q
    ft_pct <- parseJSON r
    oreb <- parseJSON s
    dreb <- parseJSON t
    reb <- parseJSON u
    ast <- parseJSON v
    stl <- parseJSON w
    blk <- parseJSON x
    tov <- parseJSON y
    pf <- parseJSON z
    pts <- parseJSON a1
    return GameResult{..}




--will use this to get game logs.


