{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, LambdaCase, FlexibleInstances #-}

module Main where

--import System.Environment
import Data.Aeson
import qualified Data.Map as M
--import GHC.Exts
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
--import qualified Data.ByteString.Lazy as B
--import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson.Types
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import qualified Data.List as L

jsonURL :: String
jsonURL = "http://stats.nba.com/stats/teamgamelog/?Season=2015-16&SeasonType=Regular%20Season&TeamID=1610612747"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

--main :: IO ()
--main = do
--  d <- (eitherDecode <$> getJSON) :: IO (Either String ResultSet)
--  case d of
--    Left err -> putStrLn err
--    Right res -> print res

main :: IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String (Maybe ResultSets)) --decode lifts over IO to hit the B.ByteString contained in the IO.
  case d of
    Left err -> putStrLn err
    Right ps -> putStrLn (show ps)
  --print d --this worked to print the Maybe Value
  --print $ (V.toList . parseTuple) decode d
--

--data GameLog = GameLog [GameResult] deriving (Show, Generic, Eq, Read)

--instance FromJSON GameResult where
--    parseJSON jsn = do
--      GameResult {..} <- parseJSON jsn
--      return GameResult {..}

--newtype ResultSets = ResultSets ResultSet deriving (Show, Eq, Read)

data ResultSet = ResultSet { rowSet :: Array } deriving (Show, Eq, Read)
data ResultSets = ResultSets [ResultSet] deriving (Show, Eq, Read)
--data ResultSet  = ResultSet  { rowSet :: Object}    deriving (Show, Eq, Read)

--
--instance FromJSON ResultSets where
--  parseJSON (Object o) = ResultSets <$> (o .: "resultSets")


instance FromJSON ResultSets where
  parseJSON (Object o) = ResultSets <$> (o .: "resultSets")
                         --ResultSets <$> ((o .: "resultSets") >>= (.: "rowSet"))
  --parseJSON _ = mzero

instance FromJSON ResultSet where
  parseJSON (Object o) = ResultSet <$> (o .: "rowSet")


--instance FromJSON [ResultSet] where
--  parseJSON arr = do
--      obj <- parseJSON arr
--      return obj
--
----this is the single Results
--instance FromJSON ResultSet where
--  parseJSON (Object o) = (o .: "rowSet")
--  parseJSON _ = mzero

--o is [{"names":...},{"headers":[Headers]},{"rowSet",[GameLogs]}]

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
type TeamABBR  = String
type Teams    = (TeamName,Team_ID, TeamABBR)

--will use this to get game logs.
teams :: [Teams]
teams = [ ("Atlanta Hawks", 1610612737, "ATL")
        , ("Boston Celtics", 1610612738, "BOS")
        , ("Brooklyn Nets", 1610612751, "BKN")
        , ("Charlotte Hornets", 1610612766, "CHA")
        , ("Chicago Bulls", 1610612741, "CHI")
        , ("Cleveland Cavaliers", 1610612739, "CLE")
        , ("Dallas Mavericks", 1610612742, "DAL")
        , ("Denver Nuggets", 1610612743, "DEN")
        , ("Detroit Pistons", 1610612765, "DET")
        , ("Golden State Warriors", 1610612744, "GSW")
        , ("Houston Rockets", 1610612745, "HOU")
        , ("Indiana Pacers", 1610612754, "IND")
        , ("Los Angeles Clippers", 1610612746, "LAC")
        , ("Los Angeles Lakers", 1610612747, "LAL")
        , ("Memphis Grizzlies", 1610612763, "MEM")
        , ("Miami Heat", 1610612748, "MIA")
        , ("Milwaukee Bucks", 1610612749, "MIL")
        , ("Minnesota Timberwolves", 1610612750, "MIN")
        , ("New Orleans Pelicans", 1610612740, "NOP")
        , ("New York Knicks", 1610612752, "NYK")
        , ("Oklahoma City Thunder",	1610612760, "OKC")
        , ("Orlando Magic", 1610612753, "ORL")
        , ("Philadelphia 76ers", 1610612755, "PHI")
        , ("Phoenix Suns", 1610612756, "PHX")
        , ("Portland Trail Blazers", 1610612757, "POR")
        , ("Sacramento Kings", 1610612758, "SAC")
        , ("San Antonio Spurs", 1610612759, "SAS")
        , ("Toronto Raptors", 1610612761, "TOR")
        , ("Utah Jazz", 1610612762, "UTA")
        , ("Washington Wizards", 1610612764, "WAS")
        ]
--
data GameResult = GameResult { team_ID   :: Int
                             , game_ID   :: String
                             , game_date :: String
                             , matchup   :: String
                             , wl        :: Char
                             , w         :: Int
                             , l         :: Int
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
                             } deriving (Show, Eq, Ord, Generic, Read)