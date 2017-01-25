{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, LambdaCase, FlexibleInstances, NamedFieldPuns, DataKinds #-}

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


--teamURLS :: [String]
--teamURLS = fmap makeURL [("sf;kjsf",1610612737,"salkdjflkasdflkjasdf"),("sdakhfkadsjhf",1610612741,"ksdahjfkasdjf")]
--    where makeURL = (\(_,team_id,_) -> "http://stats.nba.com/stats/teamgamelog/?Season=2015-16&SeasonType=Regular%20Season&TeamID=" ++ show team_id)

teamURLS :: [String]
teamURLS = fmap makeURL teams
    where makeURL = (\(_,team_id,_) -> "http://stats.nba.com/stats/teamgamelog/?Season=2015-16&SeasonType=Regular%20Season&TeamID=" ++ show team_id)

getJSON :: IO [B.ByteString]
getJSON = mapM simpleHttp teamURLS

--main :: IO ()
--main = do
--  d <- (eitherDecode <$> getJSON) :: IO (Either String ([Maybe FullPage])) --decode lifts over IO to hit the B.ByteString contained in the IO.
--  case d of
--    Left err -> putStrLn err
--    Right pss -> case pss of
--                 Just ps  -> putStrLn $ show $ fmap showResult ps
--                 Nothing -> putStrLn "Maybe did not go through"

main :: IO ()
main = do
  --d <- (getJSON >>= (\arr -> return (eitherDecode <$> arr))) :: IO (Either String ([Maybe FullPage]))
  ds <- getJSON
  d <- return (eitherDecode <$> ds) :: IO [Either String (Maybe FullPage)]
--  --d <- (eitherDecode <$> ds) :: IO (Either String ([Maybe FullPage])) --decode lifts over IO to hit the B.ByteString contained in the IO.
  --putStrLn . show $ d
  let x = fmap checkFullPage $ d
      y = (map read x) :: [[GameResult]]
      z = getBoxScores . DL.concat $ y
  --print z


--  putStrLn . show $ (fmap check d)
--                        where check = (\case Left err -> err
--                                             Right ds -> showResult $ ds)

--  case d of
--    Left err -> putStrLn err
--    Right pss -> case pss of
--                   Just pss ->
--                   Nothing -> putStrLn "Maybe did not go through."
--                 Just ps  -> putStrLn $ show $ fmap showResult ps
--                 Nothing -> putStrLn "Maybe did not go through"

checkFullPage :: Either String (Maybe FullPage) -> String
checkFullPage = (\case Left err -> err
                       Right ds -> case ds of
                                      Just x -> show . showResult $ x
                                      _      -> "Maybe failed somewhere.")

showResult :: FullPage -> [GameResult]
showResult = (rowSet . DL.head . resultSets)

getBoxScores :: [GameResult] -> [(GameResult,GameResult)]

--getBoxScores (x:y) = (team_ID x,y)
--getBoxScores (x:xs) = (\case { Just b -> (x,b); Nothing -> (x,x) }) $ DL.find (\y -> (game_ID x) == (game_ID y)) xs
--getBoxScores arr = DL.groupBy ((==) `on` game_ID) $ arr
--    where comp [x,y]
--            | (wl x) == 'W' = (x,y)
--            | otherwise     = (y,x)
--getBoxScores arr = DL.nub [boxscore x y | x <- arr , y <- arr, ((game_ID x) == (game_ID y)) && ((team_ID x) /= (team_ID y))]
--      where boxscore t1 t2
--                | (wl t1) == 'W' = (t1,t2)
--                | otherwise      = (t2,t1)
getBoxScores []     = []
getBoxScores (x:xs) =
    let sameGame = [boxscore a | a <- xs, (game_ID x) == (game_ID a) ]
    in getBoxScores xs ++ sameGame
        where boxscore t2
                 | (wl x) == 'W' = (x,t2)
                 | otherwise      = (t2,x)
--gatherResults :: [GameResult] -> GameLogs
--gatherResults gr = case gr of
--                     Just x ->
--                     Nothing ->

getWinningStats :: [(GameResult,GameResult)] -> WinningTeamStats
getWinningStats (x,y):xs |
    |














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
    min <- parseJSON i
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


