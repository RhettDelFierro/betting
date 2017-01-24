{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, LambdaCase, FlexibleInstances, NamedFieldPuns, DataKinds #-}

module Main where

import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Map as M
--import GHC.Exts
import qualified Data.ByteString.Lazy.Char8 as L
--import qualified Data.ByteString.Lazy as B
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Traversable as DT
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.List as DL
import Types

teamURLS :: [String]
teamURLS = fmap makeURL teams
    where makeURL = (\(_,team_id,_) -> "http://stats.nba.com/stats/teamgamelog/?Season=2015-16&SeasonType=Regular%20Season&TeamID=" ++ show team_id)

jsonURL :: String
jsonURL = "http://stats.nba.com/stats/teamgamelog/?Season=2015-16&SeasonType=Regular%20Season&TeamID=1610612747"

getJSON :: IO [B.ByteString]
getJSON = mapM simpleHttp teamURLS

main :: IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String ([Maybe FullPage])) --decode lifts over IO to hit the B.ByteString contained in the IO.
  case d of
    Left err -> putStrLn err
    Right pss -> case pss of
                 Just ps  -> putStrLn $ show $ fmap showResult ps
                 Nothing -> putStrLn "Maybe did not go through"
    --Right ps -> putStrLn (show ps)
--    Right ps -> (\case Just x -> putStrLn (show $ rowSet $ resultSets x)
--                       _      -> putStrLn "failed")
    --Right ps -> do
--        rowSet resultSets ps

showResult :: FullPage -> [GameResult]
showResult = (rowSet . DL.head . resultSets)



--gatherResults :: [GameResult] -> GameLogs
--gatherResults gr = case gr of
--                     Just x ->
--                     Nothing ->
















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


