Just (Object (fromList [("resource",String "teamgamelog"),
     ("parameters",Object (fromList [("LeagueID",Null),("DateFrom",Null),("Season",String "2015-16"),("TeamID",Number 1.610612747e9),("DateTo",Null),("SeasonType",String "Regular Season")]
                          )

     ),
     ("resultSets",Array [Object (fromList [("headers",Array [String "Team_ID",String "Game_ID",String "GAME_DATE",String "MATCHUP",String "WL",String "W",String "L",String "W_PCT",String "MIN",String "FGM",String "FGA",String "FG_PCT",String "FG3M",String "FG3A",String "FG3_PCT",String "FTM",String "FTA",String "FT_PCT",String "OREB",String "DREB",String "REB",String "AST",String "STL",String "BLK",String "TOV",String "PF",String "PTS"
                                                             ]
                                            )
                                            ,("name",String "TeamGameLog")
                                            ,("rowSet",Array [Array [Number 1.610612747e9,String "0021501228",String "APR 13, 2016",String "LAL vs. UTA",String "W",Number 17.0,Number 65.0,Number 0.207,Number 240.0,Number 41.0,Number 85.0,Number 0.482,Number 6.0,Number 25.0,Number 0.24,Number 13.0,Number 15.0,Number 0.867,Number 8.0,Number 39.0,Number 47.0,Number 19.0,Number 6.0,Number 3.0,Number 13.0,Number 17.0,Number 101.0],
                                                              Array [Number 1.610612747e9,String "0021501209",String "APR 11, 2016",String "LAL @ OKC",String "L",Number 16.0,Number 65.0,Number 0.198,Number 240.0,Number 23.0,Number 81.0,Number 0.284,Number 7.0,Number 31.0,Number 0.226,Number 26.0,Number 30.0,Number 0.867,Number 10.0,Number 38.0,Number 48.0,Number 15.0,Number 8.0,Number 1.0,Number 15.0,Number 20.0,Number 79.0],
                                                              Array [Number 1.610612747e9,String "0021501195",String "APR 10, 2016",String "LAL @ HOU",String "L",Number 16.0,Number 64.0,Number 0.2,Number 240.0,Number 41.0,Number 83.0,Number 0.494,Number 12.0,Number 28.0,Number 0.429,Number 16.0,Number 19.0,Number 0.842,Number 7.0,Number 33.0,Number 40.0,Number 25.0,Number 10.0,Number 3.0,Number 18.0,Number 15.0,Number 110.0],
                                                              Array [Number 1.610612747e9,String "0021501184",String "APR 08, 2016",String "LAL @ NOP",String "L",Number 16.0,Number 63.0,Number 0.203,Number 240.0,Number 34.0,Number 81.0,Number 0.42,Number 11.0,Number 24.0,Number 0.458,Number 23.0,Number 27.0,Number 0.852,Number 10.0,Number 24.0,Number 34.0,Number 18.0,Number 9.0,Number 5.0,Number 13.0,Number 23.0,Number 102.0]
                                                             ]
                                            ])
                                            ]
                                  )
                          ]
     )]
            )
     )

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