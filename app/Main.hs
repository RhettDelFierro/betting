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



