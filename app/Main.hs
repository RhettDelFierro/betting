{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, LambdaCase, FlexibleInstances, NamedFieldPuns, DataKinds #-}

module Main where

--import System.Environment
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Map as M
--import GHC.Exts
import qualified Data.ByteString.Lazy.Char8 as L
--import qualified Data.ByteString.Lazy as B
import Data.Aeson.Types
import Data.Text as T
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

import qualified Data.List as DL

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
  d <- (eitherDecode <$> getJSON) :: IO (Either String (Maybe FullPage)) --decode lifts over IO to hit the B.ByteString contained in the IO.
--  let Just v = (decode getJSON :: IO (Maybe Value))
--  print v

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
--data GameRe     = GameRe { gameRe :: Array } deriving (Show, Eq, Read)
--newtype GameResultArr = GameResultArr [GameResult] deriving (Show, Eq, Read)
--newtype RowSet  = RowSet [GameResult] deriving (Show, Eq, Read)
--data ResultSet = ResultSet { rowSet :: Array } deriving (Show, Eq, Read)
--data ResultSets = ResultSets [RowSet] deriving (Show, Eq, Read)
--data ResultSet  = ResultSet  { rowSet :: Object}    deriving (Show, Eq, Read)
--newtype GameResults = GameResults  deriving (Show, Eq, Read)
data ResultSets     = ResultSets { name :: String, headers :: [String], rowSet :: [GameResult] } deriving (Show, Eq, Read)
data FullPage = FullPage { resultSets :: [ResultSets], resource :: String, parameters :: Object } deriving (Show, Eq, Read)

instance FromJSON FullPage where
  parseJSON = withObject "Result Sets" $ \o -> do
      resource    <- o .: "resource"
      parameters  <- o .: "parameters"
      resultSets  <- o .: "resultSets"
      return FullPage{..}

instance FromJSON ResultSets where
  parseJSON = withObject "Row Sets" $ \o -> do
      name    <- o .: "name"
      headers <- o .: "headers"
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

      --[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1] <- arr
--      x <- arr
--      return $ GameResults <$> arr
      --GameResult <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s <*> t <*> u <*> v <*> w <*> x <*> y <*> z <*> a1

--instance FromJSON ResultSets where
--  parseJSON (Object o) = ResultSets <$> (o .: "resultSets")

--instance FromJSON ResultSets where
--  parseJSON (Object o) = ResultSets <$> (o .: "resultSets")
--                         --ResultSets <$> ((o .: "resultSets") >>= (.: "rowSet"))
--  --parseJSON _ = mzero
--
--instance FromJSON RowSet where
--  parseJSON (Object o) = RowSet <$> (o .: "rowSet")
--
----instance FromJSON GameResultArr where
----  parseJSON arr = GameResultArr <$> parseJSON arr
--
--instance FromJSON GameResult where
--  parseJSON arr = do
--    [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1] <- parseJSON arr
--    GameResult <$> a <*> b <*> c <*> d <*> e <*> f <*> g <*> h <*> i <*> j <*> k <*> l <*> m <*> n <*> o <*> p <*> q <*> r <*> s <*> t <*> u <*> v <*> w <*> x <*> y <*> z <*> a1
--     --let boxscore = GameResult a b c d e f g h i j k l m n o p q r s t u v w x y z a1
--        --return boxscore


--     team_ID <- a
--     game_ID <- b
--     game_date <- c
--     matchup <- d
--     wl <- e
--     w <- f
--     l <- g
--     w_pct <- h
--     min <- i
--     fgm <- j
--     fga <- k
--     fg_pct <- l
--     fg3m <- m
--     fg3a <- n
--     fg3_pct <- o
--     ftm <- p
--     fta <- q
--     ft_pct <- r
--     oreb <- s
--     dreb <- t
--     reb <- u
--     ast <- v
--     stl <- w
--     blk <- x
--     tov <- y
--     pf <- z
--     pts <- a1
--     team_ID <- parseJSON a
--     game_ID <- parseJSON b
--     game_date <- parseJSON c
--     matchup <- parseJSON d
--     wl <- parseJSON e
--     w <- parseJSON f
--     l <- parseJSON g
--     w_pct <- parseJSON h
--     min <- parseJSON i
--     fgm <- parseJSON j
--     fga <- parseJSON k
--     fg_pct <- parseJSON l
--     fg3m <- parseJSON m
--     fg3a <- parseJSON n
--     fg3_pct <- parseJSON o
--     ftm <- parseJSON p
--     fta <- parseJSON q
--     ft_pct <- parseJSON r
--     oreb <- parseJSON s
--     dreb <- parseJSON t
--     reb <- parseJSON u
--     ast <- parseJSON v
--     stl <- parseJSON w
--     blk <- parseJSON x
--     tov <- parseJSON y
--     pf <- parseJSON z
--     pts <- parseJSON a1
--     return GameResult{..}
--     return GameResult { team_ID = team_ID <$> a
--                       }
--makeGameResult :: [Value] -> GameResult
----makeGameResult arr = do
----    GameResult{..} <-
--makeGameResult = withArray "array" $ \[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1] -> do
--    team_ID <- parseJSON a
--    game_ID <- parseJSON b
--    game_date <- parseJSON c
--    matchup <- parseJSON d
--    wl <- parseJSON e
--    w <- parseJSON f
--    l <- parseJSON g
--    w_pct <- parseJSON h
--    min <- parseJSON i
--    fgm <- parseJSON j
--    fga <- parseJSON k
--    fg_pct <- parseJSON l
--    fg3m <- parseJSON m
--    fg3a <- parseJSON n
--    fg3_pct <- parseJSON o
--    ftm <- parseJSON p
--    fta <- parseJSON q
--    ft_pct <- parseJSON r
--    oreb <- parseJSON s
--    dreb <- parseJSON t
--    reb <- parseJSON u
--    ast <- parseJSON v
--    stl <- parseJSON w
--    blk <- parseJSON x
--    tov <- parseJSON y
--    pf <- parseJSON z
--    pts <- parseJSON a1
--    GameResult{..}

--    GameResult { team_ID <$> a
--               , game_ID <$> b
--               , game_date =parseJSON c
--               , matchup <$> d
--               , wl <$> e
--               , w =parseJSON f
--               , l <$> g
--               , w_pct <$> h
--               , min <$> i
--               , fgm <$> j
--               , fga <$> k
--               , fg_pct <$> l
--               , fg3m <$> m
--               , fg3a <$> n
--               , fg3_pct <$> o
--               , ftm <$> p
--               , fta <$> q
--               , ft_pct <$> r
--               , oreb <$> s
--               , dreb <$> t
--               , reb <$> u
--               , ast <$> v
--               , stl <$> w
--               , blk <$> x
--               , tov <$> y
--               , pf <$> z
--               , pts = parsonJSON a1
--               }

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
                             } deriving (Show, Eq, Generic, Read)

--data GameResult = GameResult { team_ID   :: Int
--                             , game_ID   :: Int
--                             , game_date :: Int
--                             , matchup   :: Int
--                             , wl        :: Int
--                             , w         :: Int
--                             , l         :: Int
--                             , w_pct     :: Int
--                             , min       :: Int
--                             , fgm       :: Int
--                             , fga       :: Int
--                             , fg_pct    :: Int
--                             , fg3m      :: Int
--                             , fg3a      :: Int
--                             , fg3_pct   :: Int
--                             , ftm       :: Int
--                             , fta       :: Int
--                             , ft_pct    :: Int
--                             , oreb      :: Int
--                             , dreb      :: Int
--                             , reb       :: Int
--                             , ast       :: Int
--                             , stl       :: Int
--                             , blk       :: Int
--                             , tov       :: Int
--                             , pf        :: Int
--                             , pts       :: Int
--                             } deriving (Show, Eq, Ord, Generic, Read)
