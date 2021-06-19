Project I
================
Soohee Jung
6/11/2021

-   [FUNCTIONS](#functions)
    -   [Required Packages](#required-packages)
    -   [Record-API Functions](#record-api-functions)
    -   [Stat-API function](#stat-api-function)
    -   [Wrapper function to call the functions
        above](#wrapper-function-to-call-the-functions-above)
-   [Contingency Tables](#contingency-tables)
    -   [Franchise Records](#franchise-records)
    -   [Franchise-team-totals Records](#franchise-team-totals-records)
    -   [franchise-season-records by
        franchiseId=ID](#franchise-season-records-by-franchiseidid)
    -   [franchise-goalie-records by
        franchiseId=ID](#franchise-goalie-records-by-franchiseidid)
    -   [franchise-skater-records by
        franchiseId=ID](#franchise-skater-records-by-franchiseidid)
    -   [franchise-detail records by
        mostRecentTeamId=ID](#franchise-detail-records-by-mostrecentteamidid)
    -   [Team Stat modifier](#team-stat-modifier)
-   [Getting basic ideas](#getting-basic-ideas)
    -   [Choose two franchises to compare
        with](#choose-two-franchises-to-compare-with)
    -   [Goals by skater position](#goals-by-skater-position)
    -   [Assists by skater position](#assists-by-skater-position)
-   [FACTORS WHICH INFLUENCE TEAM
    WINNING](#factors-which-influence-team-winning)
    -   [How skater assists affect
        winning?](#how-skater-assists-affect-winning)
    -   [How center position skater’s penalty time affect
        goal?](#how-center-position-skaters-penalty-time-affect-goal)
    -   [Is playing at Home really an
        advantage?](#is-playing-at-home-really-an-advantage)
    -   [Stat](#stat)

# FUNCTIONS

## Required Packages

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)
```

## Record-API Functions

``` r
# to mapping Franchise ids vs Full names vs Most recent team ID
frurl <- GET("https://records.nhl.com/site/api/franchise")
frtext <- content(frurl, "text", encoding = "UTF-8")
frlist <- fromJSON(frtext, flatten=TRUE)
frlist <- as.data.frame(frlist)
frtbl <- tibble(frlist$data.id, frlist$data.fullName, frlist$data.mostRecentTeamId)
# print to see what it looks like
head(frtbl)
```

    ## # A tibble: 6 x 3
    ##   `frlist$data.id` `frlist$data.fullName` `frlist$data.mostRecentTeamId`
    ##              <int> <chr>                                           <int>
    ## 1                1 Montréal Canadiens                                  8
    ## 2                2 Montreal Wanderers                                 41
    ## 3                3 St. Louis Eagles                                   45
    ## 4                4 Hamilton Tigers                                    37
    ## 5                5 Toronto Maple Leafs                                10
    ## 6                6 Boston Bruins                                       6

``` r
# record API function
rcdURL <- function(list,recd,type,id){
  if (missing(recd) & missing(type) & missing(id)){
    rcdurl <- paste0("https://records.nhl.com/site/api/",list)
  }
  else if (missing(type) & missing(id)){
    rcdurl <- paste0("https://records.nhl.com/site/api/",list,"-",recd)
  }
  else {
    if (is.numeric(id)){
      rcdurl <- paste0("https://records.nhl.com/site/api/",list,"-",recd,"?cayenneExp=", type, "=", id)
    }
    else {
      if (type=="mostRecentTeamId"){
        id <- filter(filter(frtbl,frtbl[2]==id)[3])
        rcdurl <- paste0("https://records.nhl.com/site/api/",list,"-",recd,"?cayenneExp=", type, "=", id)
      }
      else {
        id <- filter(filter(frtbl,frtbl[2]==id)[1])
        rcdurl <- paste0("https://records.nhl.com/site/api/",list,"-",recd,"?cayenneExp=", type, "=", id)
      }
    }
  }
  return(rcdurl)
}

rcddt <- function(list,...){
  rcdNHL <- GET(rcdURL(list,...))
  rcdtext <- content(rcdNHL, "text",encoding = "UTF-8")
  rcdlist <- fromJSON(rcdtext, flatten=TRUE)
  rcdlist <- as.data.frame(rcdlist)
  return(rcdlist)
}
```

## Stat-API function

``` r
statURL <- function(list,id){
  if (missing(id)){
    staturl <- paste0("https://statsapi.web.nhl.com/api/v1/",list,"?expand=team.stats")
  }
  else{
    if (is.numeric(id)){
      staturl <- paste0("https://statsapi.web.nhl.com/api/v1/",list,"/",id,"?expand=team.stats")
    }
    else {
      id <- filter(filter(frtbl,frtbl[2]==id)[1])
      staturl <- paste0("https://statsapi.web.nhl.com/api/v1/",list,"/",id,"?expand=team.stats")
    }
  }
  return(staturl)
}

statdt <- function(list,...){
  statNHL <- GET(statURL(list,...))
  stattext <- content(statNHL, "text",encoding = "UTF-8")
  statlist <- fromJSON(stattext, flatten=TRUE)
  statlist <- as.data.frame(statlist)
  statlist <- unnest(unnest(statlist,cols = c(teams.teamStats)),cols = c(splits))
  return(statlist)
}
```

## Wrapper function to call the functions above

``` r
# choose record or stat and then put parameters what we want
wrapfnc <- function(fnc,list,...){
  if (fnc=="record"){
    return(rcddt(list,...))
  }
  else if (fnc=="stat"){
    return(statdt(list,...))
  }
  else cat("choose record or stat!!")
}
```

# Contingency Tables

## Franchise Records

``` r
as.tbl(wrapfnc("record","franchise"))
```

    ## # A tibble: 39 x 9
    ##    data.id data.firstSeaso~ data.fullName data.lastSeason~ data.mostRecent~
    ##      <int>            <int> <chr>                    <int>            <int>
    ##  1       1         19171918 Montréal Can~               NA                8
    ##  2       2         19171918 Montreal Wan~         19171918               41
    ##  3       3         19171918 St. Louis Ea~         19341935               45
    ##  4       4         19191920 Hamilton Tig~         19241925               37
    ##  5       5         19171918 Toronto Mapl~               NA               10
    ##  6       6         19241925 Boston Bruins               NA                6
    ##  7       7         19241925 Montreal Mar~         19371938               43
    ##  8       8         19251926 Brooklyn Ame~         19411942               51
    ##  9       9         19251926 Philadelphia~         19301931               39
    ## 10      10         19261927 New York Ran~               NA                3
    ## # ... with 29 more rows, and 4 more variables: data.teamAbbrev <chr>,
    ## #   data.teamCommonName <chr>, data.teamPlaceName <chr>, total <int>

## Franchise-team-totals Records

``` r
head(wrapfnc("record","franchise","team-totals"))
```

    ##   data.id data.activeFranchise data.firstSeasonId data.franchiseId
    ## 1       1                    1           19821983               23
    ## 2       2                    1           19821983               23
    ## 3       3                    1           19721973               22
    ## 4       4                    1           19721973               22
    ## 5       5                    1           19261927               10
    ## 6       6                    1           19261927               10
    ##   data.gameTypeId data.gamesPlayed data.goalsAgainst data.goalsFor
    ## 1               2             2993              8902          8792
    ## 2               3              257               634           697
    ## 3               2             3788             11907         12045
    ## 4               3              309               897           983
    ## 5               2             6560             20020         20041
    ## 6               3              518              1447          1404
    ##   data.homeLosses data.homeOvertimeLosses data.homeTies data.homeWins
    ## 1             525                      85            96           790
    ## 2              53                       0            NA            74
    ## 3             678                      84           170           963
    ## 4              53                       1            NA            94
    ## 5            1143                      76           448          1614
    ## 6             104                       0             1           137
    ##   data.lastSeasonId data.losses data.overtimeLosses data.penaltyMinutes
    ## 1                NA        1211                 169               44773
    ## 2                NA         120                   0                4266
    ## 3                NA        1587                 166               57792
    ## 4                NA         139                   0                5689
    ## 5                NA        2716                 153               86129
    ## 6                NA         266                   0                8181
    ##   data.pointPctg data.points data.roadLosses data.roadOvertimeLosses
    ## 1         0.5306        3176             686                      84
    ## 2         0.0039           2              67                       0
    ## 3         0.5133        3889             909                      82
    ## 4         0.0129           8              86                       2
    ## 5         0.5127        6727            1573                      77
    ## 6         0.0000           0             162                       0
    ##   data.roadTies data.roadWins data.shootoutLosses data.shootoutWins
    ## 1           123           604                  84                78
    ## 2            NA            63                   0                 0
    ## 3           177           725                  70                86
    ## 4            NA            76                   0                 0
    ## 5           360          1269                  68                79
    ## 6             7           107                   0                 0
    ##   data.shutouts data.teamId      data.teamName data.ties data.triCode
    ## 1           196           1  New Jersey Devils       219          NJD
    ## 2            25           1  New Jersey Devils        NA          NJD
    ## 3           177           2 New York Islanders       347          NYI
    ## 4            12           2 New York Islanders        NA          NYI
    ## 5           408           3   New York Rangers       808          NYR
    ## 6            44           3   New York Rangers         8          NYR
    ##   data.wins total
    ## 1      1394   105
    ## 2       137   105
    ## 3      1688   105
    ## 4       170   105
    ## 5      2883   105
    ## 6       244   105

## franchise-season-records by franchiseId=ID

``` r
# I choose ID=10
head(wrapfnc("record","franchise","season-records","franchiseId",10))
```

    ##   data.id data.fewestGoals data.fewestGoalsAgainst
    ## 1       3              150                     177
    ##   data.fewestGoalsAgainstSeasons data.fewestGoalsSeasons data.fewestLosses
    ## 1                   1970-71 (78)            1954-55 (70)                17
    ##   data.fewestLossesSeasons data.fewestPoints data.fewestPointsSeasons
    ## 1             1971-72 (78)                47             1965-66 (70)
    ##   data.fewestTies data.fewestTiesSeasons data.fewestWins
    ## 1               4           2001-02 (82)              17
    ##                     data.fewestWinsSeasons data.franchiseId
    ## 1 1952-53 (70), 1954-55 (70), 1959-60 (70)               10
    ##   data.franchiseName data.homeLossStreak
    ## 1   New York Rangers                   7
    ##                               data.homeLossStreakDates
    ## 1 Oct 20 1976 - Nov 14 1976, Mar 24 1993 - Apr 14 1993
    ##   data.homePointStreak
    ## 1                   24
    ##                              data.homePointStreakDates data.homeWinStreak
    ## 1 Oct 14 1970 - Jan 31 1971, Oct 24 1995 - Feb 15 1996                 14
    ##     data.homeWinStreakDates data.homeWinlessStreak
    ## 1 Dec 19 1939 - Feb 25 1940                     10
    ##   data.homeWinlessStreakDates data.lossStreak      data.lossStreakDates
    ## 1   Jan 30 1944 - Mar 19 1944              11 Oct 30 1943 - Nov 27 1943
    ##   data.mostGameGoals      data.mostGameGoalsDates data.mostGoals
    ## 1                 12 Nov 21 1971 - CGS 1 @ NYR 12            321
    ##   data.mostGoalsAgainst data.mostGoalsAgainstSeasons data.mostGoalsSeasons
    ## 1                   345                 1984-85 (80)          1991-92 (80)
    ##   data.mostLosses data.mostLossesSeasons data.mostPenaltyMinutes
    ## 1              44           1984-85 (80)                    2021
    ##   data.mostPenaltyMinutesSeasons data.mostPoints data.mostPointsSeasons
    ## 1                   1989-90 (80)             113           2014-15 (82)
    ##   data.mostShutouts data.mostShutoutsSeasons data.mostTies
    ## 1                13             1928-29 (44)            21
    ##   data.mostTiesSeasons data.mostWins data.mostWinsSeasons data.pointStreak
    ## 1         1950-51 (70)            53         2014-15 (82)               19
    ##       data.pointStreakDates data.roadLossStreak
    ## 1 Nov 23 1939 - Jan 13 1940                  10
    ##                               data.roadLossStreakDates
    ## 1 Oct 30 1943 - Dec 23 1943, Feb 08 1961 - Mar 15 1961
    ##   data.roadPointStreak data.roadPointStreakDates data.roadWinStreak
    ## 1                   11 Nov 05 1939 - Jan 13 1940                  9
    ##     data.roadWinStreakDates data.roadWinlessStreak
    ## 1 Jan 16 2020 - Feb 27 2020                     16
    ##   data.roadWinlessStreakDates data.winStreak
    ## 1   Oct 09 1952 - Dec 20 1952             10
    ##                                    data.winStreakDates data.winlessStreak
    ## 1 Dec 19 1939 - Jan 13 1940, Jan 19 1973 - Feb 10 1973                 21
    ##     data.winlessStreakDates total
    ## 1 Jan 23 1944 - Mar 19 1944     1

``` r
# I choose Franchise full name="New Jersey Devils"
head(wrapfnc("record","franchise","season-records","franchiseId","New Jersey Devils"))
```

    ##   data.id data.fewestGoals data.fewestGoalsAgainst
    ## 1       1              174                     164
    ##   data.fewestGoalsAgainstSeasons data.fewestGoalsSeasons data.fewestLosses
    ## 1                   2003-04 (82)            2010-11 (82)                19
    ##   data.fewestLossesSeasons data.fewestPoints data.fewestPointsSeasons
    ## 1             2000-01 (82)                36             1975-76 (80)
    ##   data.fewestTies data.fewestTiesSeasons data.fewestWins
    ## 1               3           1985-86 (80)              12
    ##   data.fewestWinsSeasons data.franchiseId data.franchiseName
    ## 1           1975-76 (80)               23  New Jersey Devils
    ##   data.homeLossStreak  data.homeLossStreakDates data.homePointStreak
    ## 1                  10 Jan 26 2021 - Mar 13 2021                   15
    ##                              data.homePointStreakDates data.homeWinStreak
    ## 1 Jan 08 1997 - Mar 15 1997, Dec 14 1999 - Feb 17 2000                 11
    ##     data.homeWinStreakDates data.homeWinlessStreak
    ## 1 Feb 09 2009 - Mar 20 2009                     14
    ##                            data.homeWinlessStreakDates data.lossStreak
    ## 1 Feb 12 1976 - Mar 30 1976, Feb 04 1979 - Mar 31 1979              14
    ##        data.lossStreakDates data.mostGameGoals
    ## 1 Dec 30 1975 - Jan 29 1976                  9
    ##                                                                                                                                                                                                                                               data.mostGameGoalsDates
    ## 1 Apr 01 1979 - STL 5 @ CLR 9, Feb 12 1982 - QUE 2 @ CLR 9, Apr 06 1986 - NYI 7 @ NJD 9, Mar 10 1990 - QUE 3 @ NJD 9, Dec 05 1990 - VAN 4 @ NJD 9, Oct 26 1991 - SJS 0 @ NJD 9, Mar 23 1993 - TBL 3 @ NJD 9, Mar 10 2000 - NJD 9 @ ATL 0, Oct 28 2000 - NJD 9 @ PIT 0
    ##   data.mostGoals data.mostGoalsAgainst data.mostGoalsAgainstSeasons
    ## 1            308                   374                 1985-86 (80)
    ##   data.mostGoalsSeasons data.mostLosses     data.mostLossesSeasons
    ## 1          1992-93 (84)              56 1975-76 (80), 1983-84 (80)
    ##   data.mostPenaltyMinutes data.mostPenaltyMinutesSeasons data.mostPoints
    ## 1                    2494                   1988-89 (80)             111
    ##   data.mostPointsSeasons data.mostShutouts data.mostShutoutsSeasons
    ## 1           2000-01 (82)                14             2003-04 (82)
    ##   data.mostTies data.mostTiesSeasons data.mostWins data.mostWinsSeasons
    ## 1            21         1977-78 (80)            51         2008-09 (82)
    ##   data.pointStreak     data.pointStreakDates data.roadLossStreak
    ## 1               16 Dec 26 1999 - Jan 28 2000                  12
    ##    data.roadLossStreakDates data.roadPointStreak
    ## 1 Oct 19 1983 - Dec 01 1983                   10
    ##                              data.roadPointStreakDates data.roadWinStreak
    ## 1 Feb 27 2001 - Apr 07 2001, Jan 30 2007 - Mar 15 2007                 10
    ##     data.roadWinStreakDates data.roadWinlessStreak
    ## 1 Feb 27 2001 - Apr 07 2001                     32
    ##   data.roadWinlessStreakDates data.winStreak       data.winStreakDates
    ## 1   Nov 12 1977 - Mar 15 1978             13 Feb 26 2001 - Mar 23 2001
    ##   data.winlessStreak   data.winlessStreakDates total
    ## 1                 27 Feb 12 1976 - Apr 04 1976     1

## franchise-goalie-records by franchiseId=ID

``` r
# I choose ID=20
head(wrapfnc("record","franchise","goalie-records","franchiseId",20))
```

    ##   data.id data.activePlayer data.firstName data.franchiseId
    ## 1     304             FALSE        Richard               20
    ## 2     364             FALSE           Gary               20
    ## 3     367             FALSE           Sean               20
    ## 4    1224             FALSE          Frank               20
    ## 5     373             FALSE        Jacques               20
    ## 6     406             FALSE            Bob               20
    ##   data.franchiseName data.gameTypeId data.gamesPlayed data.lastName
    ## 1  Vancouver Canucks               2              377       Brodeur
    ## 2  Vancouver Canucks               2               73       Bromley
    ## 3  Vancouver Canucks               2               16         Burke
    ## 4  Vancouver Canucks               2              102       Caprice
    ## 5  Vancouver Canucks               2               10         Caron
    ## 6  Vancouver Canucks               2               39       Essensa
    ##   data.losses         data.mostGoalsAgainstDates
    ## 1         173                         1981-10-17
    ## 2          27             1981-02-20, 1979-03-08
    ## 3           9 1998-01-28, 1998-01-21, 1998-01-15
    ## 4          46                         1985-11-08
    ## 5           5                         1973-12-20
    ## 6          12             2001-02-17, 2000-12-29
    ##   data.mostGoalsAgainstOneGame data.mostSavesDates data.mostSavesOneGame
    ## 1                           10          1985-02-10                    51
    ## 2                            9          1979-03-15                    41
    ## 3                            6          1998-01-26                    33
    ## 4                           13          1983-12-18                    41
    ## 5                            9          1973-11-27                    33
    ## 6                            5          2001-03-13                    37
    ##   data.mostShotsAgainstDates data.mostShotsAgainstOneGame
    ## 1                 1985-02-10                           54
    ## 2                 1979-03-15                           45
    ## 3     1998-01-28, 1998-01-26                           36
    ## 4     1984-11-17, 1984-10-13                           45
    ## 5                 1973-12-20                           39
    ## 6                 2001-03-13                           39
    ##   data.mostShutoutsOneSeason data.mostShutoutsSeasonIds
    ## 1                          2         19811982, 19851986
    ## 2                          2                   19781979
    ## 3                          0                   19971998
    ## 4                          1                   19831984
    ## 5                          0                   19731974
    ## 6                          1                   20002001
    ##   data.mostWinsOneSeason       data.mostWinsSeasonIds data.overtimeLosses
    ## 1                     21                     19821983                  NA
    ## 2                     11                     19781979                  NA
    ## 3                      2                     19971998                  NA
    ## 4                      8 19831984, 19841985, 19861987                  NA
    ## 5                      2                     19731974                  NA
    ## 6                     18                     20002001                  NA
    ##   data.playerId data.positionCode data.rookieGamesPlayed
    ## 1       8445694                 G                     NA
    ## 2       8445695                 G                     NA
    ## 3       8445769                 G                     NA
    ## 4       8445919                 G                     28
    ## 5       8445966                 G                     NA
    ## 6       8446719                 G                     NA
    ##   data.rookieShutouts data.rookieWins data.seasons data.shutouts data.ties
    ## 1                  NA              NA            8             6        62
    ## 2                  NA              NA            3             3        14
    ## 3                  NA              NA            1             0         4
    ## 4                   1               8            6             1        11
    ## 5                  NA              NA            1             0         1
    ## 6                  NA              NA            1             1         3
    ##   data.wins total
    ## 1       126    40
    ## 2        25    40
    ## 3         2    40
    ## 4        31    40
    ## 5         2    40
    ## 6        18    40

``` r
# I choose Franchise full name="Philadelphia Flyers"
head(wrapfnc("record","franchise","goalie-records","franchiseId","Philadelphia Flyers"))
```

    ##   data.id data.activePlayer data.firstName data.franchiseId
    ## 1     341             FALSE       Stephane               16
    ## 2     366             FALSE           Sean               16
    ## 3     440             FALSE           Jeff               16
    ## 4     239             FALSE            Ron               16
    ## 5     466             FALSE           Gary               16
    ## 6     482             FALSE           Mark               16
    ##    data.franchiseName data.gameTypeId data.gamesPlayed data.lastName
    ## 1 Philadelphia Flyers               2               16    Beauregard
    ## 2 Philadelphia Flyers               2               26         Burke
    ## 3 Philadelphia Flyers               2               27       Hackett
    ## 4 Philadelphia Flyers               2              489       Hextall
    ## 5 Philadelphia Flyers               2                8        Inness
    ## 6 Philadelphia Flyers               2               38      Laforest
    ##   data.losses data.mostGoalsAgainstDates data.mostGoalsAgainstOneGame
    ## 1           9                 1992-11-28                            9
    ## 2           8     2004-02-17, 2004-02-16                            5
    ## 3          10                 2004-01-13                            6
    ## 4         172                 1987-04-05                            9
    ## 5           0                 1976-10-17                            4
    ## 6          16     1989-01-15, 1988-10-22                            8
    ##                  data.mostSavesDates data.mostSavesOneGame
    ## 1 1992-12-12, 1992-11-28, 1992-11-04                    31
    ## 2                         2004-02-26                    40
    ## 3                         2003-12-06                    38
    ## 4                         1990-12-23                    45
    ## 5                         1976-03-25                    36
    ## 6                         1988-03-25                    47
    ##   data.mostShotsAgainstDates data.mostShotsAgainstOneGame
    ## 1                 1992-11-28                           40
    ## 2     2004-02-26, 2004-02-19                           41
    ## 3                 2003-12-06                           39
    ## 4                 1988-10-13                           50
    ## 5                 1976-03-25                           37
    ## 6                 1988-03-25                           51
    ##   data.mostShutoutsOneSeason data.mostShutoutsSeasonIds
    ## 1                          0                   19921993
    ## 2                          1         19971998, 20032004
    ## 3                          3                   20032004
    ## 4                          5                   19961997
    ## 5                          0         19751976, 19761977
    ## 6                          1                   19871988
    ##   data.mostWinsOneSeason data.mostWinsSeasonIds data.overtimeLosses
    ## 1                      3               19921993                  NA
    ## 2                      7               19971998                  NA
    ## 3                     10               20032004                  NA
    ## 4                     37               19861987                  NA
    ## 5                      2               19751976                  NA
    ## 6                      5     19871988, 19881989                  NA
    ##   data.playerId data.positionCode data.rookieGamesPlayed
    ## 1       8445382                 G                     NA
    ## 2       8445769                 G                     NA
    ## 3       8447449                 G                     NA
    ## 4       8447775                 G                     66
    ## 5       8448175                 G                     NA
    ## 6       8448628                 G                     NA
    ##   data.rookieShutouts data.rookieWins data.seasons data.shutouts data.ties
    ## 1                  NA              NA            1             0         0
    ## 2                  NA              NA            2             2         2
    ## 3                  NA              NA            1             3         6
    ## 4                   1              37           11            18        58
    ## 5                  NA              NA            2             0         2
    ## 6                  NA              NA            2             1         4
    ##   data.wins total
    ## 1         3    34
    ## 2        13    34
    ## 3        10    34
    ## 4       240    34
    ## 5         3    34
    ## 6        10    34

## franchise-skater-records by franchiseId=ID

``` r
# I choose ID=30
head(wrapfnc("record","franchise","skater-records","franchiseId",30))
```

    ##   data.id data.activePlayer data.assists data.firstName data.franchiseId
    ## 1   18785             FALSE            0            Joe               30
    ## 2   19225             FALSE            0          Bobby               30
    ## 3   19982             FALSE            1           Marc               30
    ## 4   22168             FALSE            2          Steve               30
    ## 5   22256             FALSE            1            Jim               30
    ## 6   22437             FALSE            1        Dominic               30
    ##   data.franchiseName data.gameTypeId data.gamesPlayed data.goals
    ## 1    Ottawa Senators               2                6          0
    ## 2    Ottawa Senators               2                1          0
    ## 3    Ottawa Senators               2               10          0
    ## 4    Ottawa Senators               2                8          0
    ## 5    Ottawa Senators               2                4          0
    ## 6    Ottawa Senators               2                2          0
    ##   data.lastName
    ## 1       Cirella
    ## 2        Dollas
    ## 3       Fortier
    ## 4       Konroyd
    ## 5          Kyte
    ## 6        Lavoie
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                data.mostAssistsGameDates
    ## 1 1995-10-07, 1995-10-13, 1995-10-15, 1995-10-19, 1995-10-21, 1995-10-22, 1995-10-24, 1995-10-26, 1995-10-28, 1995-10-29, 1995-11-02, 1995-11-04, 1995-11-08, 1995-11-09, 1995-11-11, 1995-11-15, 1995-11-16, 1995-11-18, 1995-11-19, 1995-11-22, 1995-11-25, 1995-11-28, 1995-11-30, 1995-12-02, 1995-12-05, 1995-12-07, 1995-12-09, 1995-12-12, 1995-12-13, 1995-12-15, 1995-12-17, 1995-12-18, 1995-12-23, 1995-12-26, 1995-12-27, 1995-12-30, 1995-12-31, 1996-01-03, 1996-01-05, 1996-01-06, 1996-01-11, 1996-01-13, 1996-01-17, 1996-01-22, 1996-01-24, 1996-01-25, 1996-01-27, 1996-01-29, 1996-01-31, 1996-02-01, 1996-02-03, 1996-02-06, 1996-02-08, 1996-02-10, 1996-02-12, 1996-02-15, 1996-02-17, 1996-02-20, 1996-02-22, 1996-02-25, 1996-02-28, 1996-03-01, 1996-03-02, 1996-03-07, 1996-03-09, 1996-03-13, 1996-03-15, 1996-03-17, 1996-03-19, 1996-03-21, 1996-03-22, 1996-03-24, 1996-03-27, 1996-03-29, 1996-03-30, 1996-04-01, 1996-04-03, 1996-04-05, 1996-04-06, 1996-04-10, 1996-04-11, 1996-04-13
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1999-11-10, 1999-11-11
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1992-10-08
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1994-03-23, 1994-03-30
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1992-12-07
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1992-11-15
    ##   data.mostAssistsOneGame data.mostAssistsOneSeason
    ## 1                       0                         0
    ## 2                       0                         0
    ## 3                       1                         1
    ## 4                       1                         2
    ## 5                       1                         1
    ## 6                       1                         1
    ##   data.mostAssistsSeasonIds
    ## 1                  19951996
    ## 2                  19992000
    ## 3                  19921993
    ## 4                  19931994
    ## 5                  19921993
    ## 6                  19921993
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  data.mostGoalsGameDates
    ## 1 1995-10-07, 1995-10-13, 1995-10-15, 1995-10-19, 1995-10-21, 1995-10-22, 1995-10-24, 1995-10-26, 1995-10-28, 1995-10-29, 1995-11-02, 1995-11-04, 1995-11-08, 1995-11-09, 1995-11-11, 1995-11-15, 1995-11-16, 1995-11-18, 1995-11-19, 1995-11-22, 1995-11-25, 1995-11-28, 1995-11-30, 1995-12-02, 1995-12-05, 1995-12-07, 1995-12-09, 1995-12-12, 1995-12-13, 1995-12-15, 1995-12-17, 1995-12-18, 1995-12-23, 1995-12-26, 1995-12-27, 1995-12-30, 1995-12-31, 1996-01-03, 1996-01-05, 1996-01-06, 1996-01-11, 1996-01-13, 1996-01-17, 1996-01-22, 1996-01-24, 1996-01-25, 1996-01-27, 1996-01-29, 1996-01-31, 1996-02-01, 1996-02-03, 1996-02-06, 1996-02-08, 1996-02-10, 1996-02-12, 1996-02-15, 1996-02-17, 1996-02-20, 1996-02-22, 1996-02-25, 1996-02-28, 1996-03-01, 1996-03-02, 1996-03-07, 1996-03-09, 1996-03-13, 1996-03-15, 1996-03-17, 1996-03-19, 1996-03-21, 1996-03-22, 1996-03-24, 1996-03-27, 1996-03-29, 1996-03-30, 1996-04-01, 1996-04-03, 1996-04-05, 1996-04-06, 1996-04-10, 1996-04-11, 1996-04-13
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1999-11-10, 1999-11-11
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1992-10-08, 1992-10-10, 1992-10-12, 1992-10-14, 1992-10-16, 1992-10-20, 1992-10-22, 1992-10-24, 1992-10-27, 1992-10-30, 1992-10-31, 1992-11-03, 1992-11-05, 1992-11-06, 1992-11-09, 1992-11-11, 1992-11-13, 1992-11-15, 1992-11-17, 1992-11-19, 1992-11-21, 1992-11-23, 1992-11-25, 1992-11-27, 1992-11-29, 1992-12-01, 1992-12-03, 1992-12-05, 1992-12-07, 1992-12-09, 1992-12-10, 1992-12-12, 1992-12-15, 1992-12-17, 1992-12-26, 1992-12-27
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     1994-03-23, 1994-03-24, 1994-03-28, 1994-03-30, 1994-04-02, 1994-04-06, 1994-04-07, 1994-04-09, 1994-04-11, 1994-04-13, 1994-04-14
    ## 5                                                                                                                                                                                                                                                                         1992-11-29, 1992-12-01, 1992-12-03, 1992-12-05, 1992-12-07, 1992-12-09, 1992-12-10, 1992-12-12, 1992-12-15, 1992-12-17, 1992-12-19, 1992-12-21, 1992-12-23, 1992-12-26, 1992-12-27, 1992-12-31, 1993-01-02, 1993-01-06, 1993-01-08, 1993-01-10, 1993-01-12, 1993-01-14, 1993-01-16, 1993-01-17, 1993-01-19, 1993-01-21, 1993-01-23, 1993-01-26, 1993-01-28, 1993-01-30, 1993-02-01, 1993-02-03, 1993-02-08, 1993-02-09, 1993-02-13, 1993-02-17, 1993-02-20, 1993-02-22, 1993-02-23, 1993-02-25, 1993-02-27, 1993-02-28, 1993-03-02, 1993-03-04, 1993-03-07, 1993-03-13, 1993-03-18, 1993-03-22, 1993-03-25, 1993-03-27, 1993-03-28, 1993-03-30, 1993-04-01, 1993-04-03, 1993-04-04, 1993-04-07, 1993-04-10, 1993-04-11, 1993-04-13, 1993-04-14
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1992-11-13, 1992-11-15, 1992-11-17, 1992-11-19, 1992-11-21
    ##   data.mostGoalsOneGame data.mostGoalsOneSeason data.mostGoalsSeasonIds
    ## 1                     0                       0                19951996
    ## 2                     0                       0                19992000
    ## 3                     0                       0                19921993
    ## 4                     0                       0                19931994
    ## 5                     0                       0                19921993
    ## 6                     0                       0                19921993
    ##   data.mostPenaltyMinutesOneSeason data.mostPenaltyMinutesSeasonIds
    ## 1                                4                         19951996
    ## 2                                0                         19992000
    ## 3                                6                         19921993
    ## 4                                2                         19931994
    ## 5                                4                         19921993
    ## 6                                0                         19921993
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 data.mostPointsGameDates
    ## 1 1995-10-07, 1995-10-13, 1995-10-15, 1995-10-19, 1995-10-21, 1995-10-22, 1995-10-24, 1995-10-26, 1995-10-28, 1995-10-29, 1995-11-02, 1995-11-04, 1995-11-08, 1995-11-09, 1995-11-11, 1995-11-15, 1995-11-16, 1995-11-18, 1995-11-19, 1995-11-22, 1995-11-25, 1995-11-28, 1995-11-30, 1995-12-02, 1995-12-05, 1995-12-07, 1995-12-09, 1995-12-12, 1995-12-13, 1995-12-15, 1995-12-17, 1995-12-18, 1995-12-23, 1995-12-26, 1995-12-27, 1995-12-30, 1995-12-31, 1996-01-03, 1996-01-05, 1996-01-06, 1996-01-11, 1996-01-13, 1996-01-17, 1996-01-22, 1996-01-24, 1996-01-25, 1996-01-27, 1996-01-29, 1996-01-31, 1996-02-01, 1996-02-03, 1996-02-06, 1996-02-08, 1996-02-10, 1996-02-12, 1996-02-15, 1996-02-17, 1996-02-20, 1996-02-22, 1996-02-25, 1996-02-28, 1996-03-01, 1996-03-02, 1996-03-07, 1996-03-09, 1996-03-13, 1996-03-15, 1996-03-17, 1996-03-19, 1996-03-21, 1996-03-22, 1996-03-24, 1996-03-27, 1996-03-29, 1996-03-30, 1996-04-01, 1996-04-03, 1996-04-05, 1996-04-06, 1996-04-10, 1996-04-11, 1996-04-13
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1999-11-10, 1999-11-11
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1992-10-08
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1994-03-23, 1994-03-30
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1992-12-07
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1992-11-15
    ##   data.mostPointsOneGame data.mostPointsOneSeason data.mostPointsSeasonIds
    ## 1                      0                        0                 19951996
    ## 2                      0                        0                 19992000
    ## 3                      1                        1                 19921993
    ## 4                      1                        2                 19931994
    ## 5                      1                        1                 19921993
    ## 6                      1                        1                 19921993
    ##   data.penaltyMinutes data.playerId data.points data.positionCode
    ## 1                   4       8446062           0                 D
    ## 2                   0       8446435           0                 D
    ## 3                   6       8446935           1                 C
    ## 4                   2       8448521           2                 D
    ## 5                   4       8448577           1                 D
    ## 6                   0       8448717           1                 D
    ##   data.rookieGamesPlayed data.rookiePoints data.seasons total
    ## 1                     NA                NA            1   364
    ## 2                     NA                NA            1   364
    ## 3                     NA                NA            1   364
    ## 4                     NA                NA            1   364
    ## 5                     NA                NA            1   364
    ## 6                     NA                NA            1   364

``` r
# I choose Franchise full name="New York Rangers"
head(wrapfnc("record","franchise","skater-records","franchiseId","New York Rangers"))
```

    ##   data.id data.activePlayer data.assists data.firstName data.franchiseId
    ## 1   17208             FALSE            1           Doug               10
    ## 2   17216             FALSE            0          Lloyd               10
    ## 3   17230             FALSE            1           Bill               10
    ## 4   17310             FALSE            0            Hub               10
    ## 5   17360             FALSE            0            Ron               10
    ## 6   17374             FALSE            4           Vern               10
    ##   data.franchiseName data.gameTypeId data.gamesPlayed data.goals
    ## 1   New York Rangers               2                4          0
    ## 2   New York Rangers               2                3          0
    ## 3   New York Rangers               2                1          0
    ## 4   New York Rangers               2                2          0
    ## 5   New York Rangers               2                4          0
    ## 6   New York Rangers               2               28          0
    ##   data.lastName
    ## 1          Adam
    ## 2        Ailsby
    ## 3         Allum
    ## 4        Anslow
    ## 5       Attwell
    ## 6         Ayres
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                data.mostAssistsGameDates
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1950-02-09
    ## 2 1951-10-14, 1951-10-18, 1951-10-20, 1951-10-21, 1951-10-24, 1951-10-28, 1951-10-29, 1951-11-01, 1951-11-03, 1951-11-04, 1951-11-07, 1951-11-11, 1951-11-14, 1951-11-17, 1951-11-18, 1951-11-21, 1951-11-22, 1951-11-25, 1951-11-27, 1951-11-28, 1951-12-01, 1951-12-02, 1951-12-05, 1951-12-09, 1951-12-11, 1951-12-12, 1951-12-15, 1951-12-16, 1951-12-19, 1951-12-23, 1951-12-25, 1951-12-26, 1951-12-29, 1951-12-30, 1952-01-01, 1952-01-02, 1952-01-06, 1952-01-09, 1952-01-10, 1952-01-13, 1952-01-16, 1952-01-17, 1952-01-20, 1952-01-22, 1952-01-26, 1952-01-27, 1952-01-31, 1952-02-03, 1952-02-07, 1952-02-09, 1952-02-10, 1952-02-13, 1952-02-16, 1952-02-17, 1952-02-19, 1952-02-20, 1952-02-24, 1952-02-27, 1952-02-28, 1952-03-01, 1952-03-02, 1952-03-04, 1952-03-06, 1952-03-09, 1952-03-12, 1952-03-15, 1952-03-16, 1952-03-19, 1952-03-20, 1952-03-23
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1940-11-26
    ## 4                                                                                                                         1947-10-16, 1947-10-19, 1947-10-22, 1947-10-29, 1947-11-01, 1947-11-02, 1947-11-06, 1947-11-08, 1947-11-09, 1947-11-12, 1947-11-15, 1947-11-16, 1947-11-19, 1947-11-22, 1947-11-30, 1947-12-03, 1947-12-06, 1947-12-07, 1947-12-10, 1947-12-11, 1947-12-13, 1947-12-14, 1947-12-17, 1947-12-21, 1947-12-23, 1947-12-25, 1947-12-28, 1947-12-31, 1948-01-01, 1948-01-03, 1948-01-04, 1948-01-07, 1948-01-10, 1948-01-11, 1948-01-14, 1948-01-18, 1948-01-21, 1948-01-25, 1948-01-28, 1948-01-31, 1948-02-01, 1948-02-04, 1948-02-07, 1948-02-08, 1948-02-14, 1948-02-16, 1948-02-18, 1948-02-22, 1948-02-25, 1948-02-29, 1948-03-02, 1948-03-03, 1948-03-06, 1948-03-07, 1948-03-10, 1948-03-13, 1948-03-14, 1948-03-16, 1948-03-17, 1948-03-21
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1967-12-02, 1967-12-03, 1967-12-07, 1967-12-09, 1967-12-17, 1967-12-25
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1935-11-28, 1935-12-12, 1935-12-14, 1935-12-25
    ##   data.mostAssistsOneGame data.mostAssistsOneSeason
    ## 1                       1                         1
    ## 2                       0                         0
    ## 3                       1                         1
    ## 4                       0                         0
    ## 5                       0                         0
    ## 6                       1                         4
    ##   data.mostAssistsSeasonIds
    ## 1                  19491950
    ## 2                  19511952
    ## 3                  19401941
    ## 4                  19471948
    ## 5                  19671968
    ## 6                  19351936
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  data.mostGoalsGameDates
    ## 1 1949-10-15, 1949-10-16, 1949-10-19, 1949-10-22, 1949-10-25, 1949-10-26, 1949-10-29, 1949-10-30, 1949-11-02, 1949-11-06, 1949-11-09, 1949-11-12, 1949-11-13, 1949-11-16, 1949-11-20, 1949-11-23, 1949-11-26, 1949-11-27, 1949-11-30, 1949-12-03, 1949-12-04, 1949-12-07, 1949-12-10, 1949-12-11, 1949-12-14, 1949-12-17, 1949-12-18, 1949-12-21, 1949-12-24, 1949-12-25, 1949-12-28, 1949-12-31, 1950-01-01, 1950-01-04, 1950-01-07, 1950-01-08, 1950-01-11, 1950-01-14, 1950-01-15, 1950-01-18, 1950-01-21, 1950-01-22, 1950-01-25, 1950-01-28, 1950-01-29, 1950-02-01, 1950-02-02, 1950-02-05, 1950-02-09, 1950-02-12, 1950-02-15, 1950-02-18, 1950-02-19, 1950-02-22, 1950-02-23, 1950-02-25, 1950-02-26, 1950-03-01, 1950-03-04, 1950-03-05, 1950-03-08, 1950-03-09, 1950-03-11, 1950-03-12, 1950-03-15, 1950-03-18, 1950-03-19, 1950-03-21, 1950-03-22, 1950-03-26
    ## 2 1951-10-14, 1951-10-18, 1951-10-20, 1951-10-21, 1951-10-24, 1951-10-28, 1951-10-29, 1951-11-01, 1951-11-03, 1951-11-04, 1951-11-07, 1951-11-11, 1951-11-14, 1951-11-17, 1951-11-18, 1951-11-21, 1951-11-22, 1951-11-25, 1951-11-27, 1951-11-28, 1951-12-01, 1951-12-02, 1951-12-05, 1951-12-09, 1951-12-11, 1951-12-12, 1951-12-15, 1951-12-16, 1951-12-19, 1951-12-23, 1951-12-25, 1951-12-26, 1951-12-29, 1951-12-30, 1952-01-01, 1952-01-02, 1952-01-06, 1952-01-09, 1952-01-10, 1952-01-13, 1952-01-16, 1952-01-17, 1952-01-20, 1952-01-22, 1952-01-26, 1952-01-27, 1952-01-31, 1952-02-03, 1952-02-07, 1952-02-09, 1952-02-10, 1952-02-13, 1952-02-16, 1952-02-17, 1952-02-19, 1952-02-20, 1952-02-24, 1952-02-27, 1952-02-28, 1952-03-01, 1952-03-02, 1952-03-04, 1952-03-06, 1952-03-09, 1952-03-12, 1952-03-15, 1952-03-16, 1952-03-19, 1952-03-20, 1952-03-23
    ## 3                                                                                                                                                                                                                                                                         1940-11-02, 1940-11-10, 1940-11-16, 1940-11-19, 1940-11-23, 1940-11-26, 1940-11-28, 1940-11-30, 1940-12-01, 1940-12-05, 1940-12-08, 1940-12-10, 1940-12-13, 1940-12-15, 1940-12-19, 1940-12-22, 1940-12-25, 1940-12-28, 1940-12-29, 1940-12-31, 1941-01-01, 1941-01-04, 1941-01-05, 1941-01-07, 1941-01-09, 1941-01-12, 1941-01-14, 1941-01-16, 1941-01-19, 1941-01-21, 1941-01-26, 1941-02-02, 1941-02-04, 1941-02-06, 1941-02-09, 1941-02-11, 1941-02-13, 1941-02-15, 1941-02-16, 1941-02-18, 1941-02-23, 1941-02-25, 1941-02-27, 1941-03-01, 1941-03-02, 1941-03-04, 1941-03-09, 1941-03-16
    ## 4                                                                                                                         1947-10-16, 1947-10-19, 1947-10-22, 1947-10-29, 1947-11-01, 1947-11-02, 1947-11-06, 1947-11-08, 1947-11-09, 1947-11-12, 1947-11-15, 1947-11-16, 1947-11-19, 1947-11-22, 1947-11-30, 1947-12-03, 1947-12-06, 1947-12-07, 1947-12-10, 1947-12-11, 1947-12-13, 1947-12-14, 1947-12-17, 1947-12-21, 1947-12-23, 1947-12-25, 1947-12-28, 1947-12-31, 1948-01-01, 1948-01-03, 1948-01-04, 1948-01-07, 1948-01-10, 1948-01-11, 1948-01-14, 1948-01-18, 1948-01-21, 1948-01-25, 1948-01-28, 1948-01-31, 1948-02-01, 1948-02-04, 1948-02-07, 1948-02-08, 1948-02-14, 1948-02-16, 1948-02-18, 1948-02-22, 1948-02-25, 1948-02-29, 1948-03-02, 1948-03-03, 1948-03-06, 1948-03-07, 1948-03-10, 1948-03-13, 1948-03-14, 1948-03-16, 1948-03-17, 1948-03-21
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1967-12-02, 1967-12-03, 1967-12-07, 1967-12-09, 1967-12-17, 1967-12-25
    ## 6                                                                                                                                                                                                                                                                         1935-11-10, 1935-11-12, 1935-11-14, 1935-11-16, 1935-11-17, 1935-11-19, 1935-11-24, 1935-11-26, 1935-11-28, 1935-12-01, 1935-12-08, 1935-12-12, 1935-12-14, 1935-12-15, 1935-12-17, 1935-12-22, 1935-12-25, 1935-12-28, 1935-12-29, 1935-12-31, 1936-01-02, 1936-01-05, 1936-01-07, 1936-01-12, 1936-01-14, 1936-01-16, 1936-01-18, 1936-01-21, 1936-01-23, 1936-01-26, 1936-01-28, 1936-02-02, 1936-02-04, 1936-02-09, 1936-02-11, 1936-02-16, 1936-02-20, 1936-02-23, 1936-02-25, 1936-02-27, 1936-03-01, 1936-03-03, 1936-03-08, 1936-03-10, 1936-03-12, 1936-03-15, 1936-03-17, 1936-03-22
    ##   data.mostGoalsOneGame data.mostGoalsOneSeason data.mostGoalsSeasonIds
    ## 1                     0                       0                19491950
    ## 2                     0                       0                19511952
    ## 3                     0                       0                19401941
    ## 4                     0                       0                19471948
    ## 5                     0                       0                19671968
    ## 6                     0                       0                19351936
    ##   data.mostPenaltyMinutesOneSeason data.mostPenaltyMinutesSeasonIds
    ## 1                                0                         19491950
    ## 2                                2                         19511952
    ## 3                                0                         19401941
    ## 4                                0                         19471948
    ## 5                                2                         19671968
    ## 6                               38                         19351936
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 data.mostPointsGameDates
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1950-02-09
    ## 2 1951-10-14, 1951-10-18, 1951-10-20, 1951-10-21, 1951-10-24, 1951-10-28, 1951-10-29, 1951-11-01, 1951-11-03, 1951-11-04, 1951-11-07, 1951-11-11, 1951-11-14, 1951-11-17, 1951-11-18, 1951-11-21, 1951-11-22, 1951-11-25, 1951-11-27, 1951-11-28, 1951-12-01, 1951-12-02, 1951-12-05, 1951-12-09, 1951-12-11, 1951-12-12, 1951-12-15, 1951-12-16, 1951-12-19, 1951-12-23, 1951-12-25, 1951-12-26, 1951-12-29, 1951-12-30, 1952-01-01, 1952-01-02, 1952-01-06, 1952-01-09, 1952-01-10, 1952-01-13, 1952-01-16, 1952-01-17, 1952-01-20, 1952-01-22, 1952-01-26, 1952-01-27, 1952-01-31, 1952-02-03, 1952-02-07, 1952-02-09, 1952-02-10, 1952-02-13, 1952-02-16, 1952-02-17, 1952-02-19, 1952-02-20, 1952-02-24, 1952-02-27, 1952-02-28, 1952-03-01, 1952-03-02, 1952-03-04, 1952-03-06, 1952-03-09, 1952-03-12, 1952-03-15, 1952-03-16, 1952-03-19, 1952-03-20, 1952-03-23
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1940-11-26
    ## 4                                                                                                                         1947-10-16, 1947-10-19, 1947-10-22, 1947-10-29, 1947-11-01, 1947-11-02, 1947-11-06, 1947-11-08, 1947-11-09, 1947-11-12, 1947-11-15, 1947-11-16, 1947-11-19, 1947-11-22, 1947-11-30, 1947-12-03, 1947-12-06, 1947-12-07, 1947-12-10, 1947-12-11, 1947-12-13, 1947-12-14, 1947-12-17, 1947-12-21, 1947-12-23, 1947-12-25, 1947-12-28, 1947-12-31, 1948-01-01, 1948-01-03, 1948-01-04, 1948-01-07, 1948-01-10, 1948-01-11, 1948-01-14, 1948-01-18, 1948-01-21, 1948-01-25, 1948-01-28, 1948-01-31, 1948-02-01, 1948-02-04, 1948-02-07, 1948-02-08, 1948-02-14, 1948-02-16, 1948-02-18, 1948-02-22, 1948-02-25, 1948-02-29, 1948-03-02, 1948-03-03, 1948-03-06, 1948-03-07, 1948-03-10, 1948-03-13, 1948-03-14, 1948-03-16, 1948-03-17, 1948-03-21
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1967-12-02, 1967-12-03, 1967-12-07, 1967-12-09, 1967-12-17, 1967-12-25
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1935-11-28, 1935-12-12, 1935-12-14, 1935-12-25
    ##   data.mostPointsOneGame data.mostPointsOneSeason data.mostPointsSeasonIds
    ## 1                      1                        1                 19491950
    ## 2                      0                        0                 19511952
    ## 3                      1                        1                 19401941
    ## 4                      0                        0                 19471948
    ## 5                      0                        0                 19671968
    ## 6                      1                        4                 19351936
    ##   data.penaltyMinutes data.playerId data.points data.positionCode
    ## 1                   0       8444860           1                 L
    ## 2                   2       8444866           0                 D
    ## 3                   0       8444875           1                 D
    ## 4                   0       8444953           0                 C
    ## 5                   2       8444985           0                 R
    ## 6                  38       8444989           4                 D
    ##   data.rookieGamesPlayed data.rookiePoints data.seasons total
    ## 1                      4                 1            1   995
    ## 2                      3                 0            1   995
    ## 3                      1                 1            1   995
    ## 4                      2                 0            1   995
    ## 5                      4                 0            1   995
    ## 6                     NA                NA            1   995

## franchise-detail records by mostRecentTeamId=ID

``` r
# I choose ID=8
head(wrapfnc("record","franchise","detail","mostRecentTeamId",8))
```

    ##   data.id data.active
    ## 1       1        TRUE
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 data.captainHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Shea Weber: 2018-19 &ndash; Present</li>\r\n\t<li>Max Pacioretty: 2015-16 &ndash;&nbsp;2017-18</li>\r\n\t<li>(No Captain): 2014-15</li>\r\n\t<li>Brian Gionta: 2010-11 &ndash;&nbsp;2013-14</li>\r\n\t<li>(No Captain): 2009-10</li>\r\n\t<li>Saku Koivu: 1999-00 &ndash;&nbsp;2008-09</li>\r\n\t<li>Vincent Damphousse: 1997-98 &ndash;&nbsp;1998-99</li>\r\n\t<li>Pierre Turgeon and Vincent Damphousse: 1996-97</li>\r\n\t<li>Mike Keane and Pierre Turgeon: 1995-96</li>\r\n\t<li>Kirk Muller and Mike Keane: 1994-95</li>\r\n\t<li>Guy Carbonneau: 1990-91 &ndash;&nbsp;1993-94</li>\r\n\t<li>Guy Carbonneau and Chris Chelios: 1989-90</li>\r\n\t<li>Bob Gainey: 1981-82 &ndash;&nbsp;1988-89</li>\r\n\t<li>Serge Savard: 1979-80 &ndash;&nbsp;1980-81</li>\r\n\t<li>Yvan Cournoyer and Serge Savard: 1978-79</li>\r\n\t<li>Yvan Cournoyer: 1975-76 &ndash;&nbsp;1977-78</li>\r\n\t<li>Henri Richard: 1971-72 &ndash;&nbsp;1974-75</li>\r\n\t<li>Jean Beliveau: 1961-62 &ndash;&nbsp;1970-71</li>\r\n\t<li>Doug Harvey: 1960-61</li>\r\n\t<li>Maurice Richard: 1956-57 &ndash;&nbsp;1959-60</li>\r\n\t<li>Butch Bouchard: 1948-49 &ndash;&nbsp;1955-56</li>\r\n\t<li>Toe Blake and Bill Durnan: 1947-48</li>\r\n\t<li>Toe Blake: 1940-41 &ndash;&nbsp;1946-47</li>\r\n\t<li>Walt Buswell: 1939-40</li>\r\n\t<li>Babe Siebert: 1936-37 &ndash;&nbsp;1938-39</li>\r\n\t<li>Sylvio Mantha: 1933-34 &ndash;&nbsp;1935-36</li>\r\n\t<li>George Hainsworth: 1932-33</li>\r\n\t<li>Sylvio Mantha: 1926-27 &ndash;&nbsp;1931-32</li>\r\n\t<li>Bill Coutu: 1925-26</li>\r\n\t<li>Sprague Cleghorn: 1922-23 &ndash;&nbsp;1924-25</li>\r\n\t<li>Newsy Lalonde: 1916-17&nbsp;&ndash;&nbsp;1921-22</li>\r\n</ul>\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              data.coachingHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Dominique Ducharme: Feb. 25, 2021&nbsp;&ndash; Present</li>\r\n\t<li>Claude Julien: Feb. 18, 2017 &ndash; Feb. 23, 2021</li>\r\n\t<li>Michel Therrien: Jan. 19, 2013 &ndash; Feb. 12, 2017</li>\r\n\t<li>Randy Cunneyworth: Dec. 17, 2011 &ndash; April 7, 2012</li>\r\n\t<li>Jacques Martin: Oct. 1, 2009 &ndash; Dec. 15, 2011</li>\r\n\t<li>Bob Gainey: March 10&nbsp;&ndash; April 22, 2009</li>\r\n\t<li>Guy Carbonneau: Oct. 6, 2006 &ndash; March 8, 2009</li>\r\n\t<li>Bob Gainey: Jan. 14&nbsp;&ndash; May 2, 2006</li>\r\n\t<li>Claude Julien: Jan. 18, 2003 &ndash; Jan. 11, 2006</li>\r\n\t<li>Michel Therrien: Nov. 21, 2000 &ndash; Jan. 16, 2003</li>\r\n\t<li>Alain Vigneault: Oct. 1, 1997 &ndash; Nov. 18, 2000</li>\r\n\t<li>Mario Tremblay: Oct. 21, 1995 &ndash; April 26, 1997</li>\r\n\t<li>Jacques Laperriere: Oct. 20, 1995</li>\r\n\t<li>Jacques Demers: Oct. 6, 1992 &ndash; Oct. 14, 1995</li>\r\n\t<li>Pat Burns: Oct. 6, 1988 &ndash; May 9, 1992</li>\r\n\t<li>Jean Perron: Oct. 10, 1985 &ndash; April 26, 1988</li>\r\n\t<li>Jacques Lemaire: Feb. 25, 1984 &ndash; May 2, 1985</li>\r\n\t<li>Bob Berry: Oct. 8, 1981 &ndash; Feb. 23, 1984</li>\r\n\t<li>Claude Ruel: Dec. 14, 1979 &ndash; April 11, 1981</li>\r\n\t<li>Bernie Geoffrion: Oct. 11&nbsp;&ndash; Dec. 11, 1979</li>\r\n\t<li>Scotty Bowman: Oct. 9, 1971 &ndash; May 21, 1979</li>\r\n\t<li>Al MacNeil: Dec. 3, 1970 &ndash; May 18, 1971</li>\r\n\t<li>Claude Ruel: Oct. 12, 1968 &ndash; Dec. 2, 1970</li>\r\n\t<li>Toe Blake: Oct. 6, 1955 &ndash; May 11, 1968</li>\r\n\t<li>Dick Irvin: Nov. 3, 1940 &ndash; April 14, 1955</li>\r\n\t<li>Pit Lepine: Nov. 5, 1939 &ndash; March 17, 1940</li>\r\n\t<li>Jules Dugal: Jan. 29&nbsp;&ndash; March 26, 1939^</li>\r\n\t<li>Cecil Hart: Nov. 7, 1936 &ndash; Jan. 24, 1939</li>\r\n\t<li>Sylvio Mantha: Nov. 12, 1935 &ndash; March 19, 1936</li>\r\n\t<li>Leo Dandurand: Jan. 1&nbsp;&ndash; March 26, 1935</li>\r\n\t<li>Newsy Lalonde: Nov. 12, 1932 &ndash; Dec. 29, 1934</li>\r\n\t<li>Cecil Hart: Nov. 16, 1926 &ndash; March 29, 1932</li>\r\n\t<li>Leo Dandurand: Jan. 11, 1922 &ndash; March 16, 1926</li>\r\n\t<li>Newsy Lalonde: Dec. 19, 1917 &ndash; Jan. 7, 1922</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n\t<li>^ <em>The Canadiens named Babe Siebert head coach in the summer of 1939, but he died before the 1939-40 season began</em></li>\r\n</ul>\r\n
    ##      data.dateAwarded                                 data.directoryUrl
    ## 1 1917-11-26T00:00:00 https://www.nhl.com/canadiens/team/administration
    ##   data.firstSeasonId
    ## 1           19171918
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            data.generalManagerHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Marc Bergevin: May 2, 2012 &ndash; Present</li>\r\n\t<li>Pierre Gauthier: Feb. 8, 2010 &ndash; March 29, 2012</li>\r\n\t<li>Bob Gainey: June 2, 2003 &ndash; Feb. 8, 2010</li>\r\n\t<li>Andre Savard: Nov. 20, 2000 &ndash; June 2, 2003</li>\r\n\t<li>Rejean Houle: Oct. 21, 1995 &ndash; Nov. 20, 2000</li>\r\n\t<li>Serge Savard: April 28, 1983 &ndash; Oct. 17, 1995</li>\r\n\t<li>Irving Grundman: Sept. 4, 1978 &ndash; April 14, 1983</li>\r\n\t<li>Sam Pollock: May 15, 1964 &ndash; Sept. 4, 1978</li>\r\n\t<li>Frank Selke: July 26, 1946 &ndash; May 15, 1964</li>\r\n\t<li>Tommy Gorman: April 8, 1940 &ndash; July 26, 1946</li>\r\n\t<li>Jules Dugal: Jan. 27, 1939 &ndash; April 8, 1940</li>\r\n\t<li>Cecil Hart: July 30, 1936 &ndash; Jan. 27, 1939</li>\r\n\t<li>Ernest Savard: Sept. 17, 1935 &ndash; July 30, 1936</li>\r\n\t<li>Leo Dandurand: Nov. 2, 1921 &ndash; Sept. 17, 1935</li>\r\n\t<li>George Kennedy: 1910 &ndash; Oct. 19, 1921</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ##                                                             data.heroImageUrl
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/MTL/Price.jpg
    ##   data.mostRecentTeamId
    ## 1                     8
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            data.retiredNumbersSummary
    ## 1 <ul class="striped-list">\r\n\t<li>1 &ndash;&nbsp;Jacques Plante (1952-63)</li>\r\n\t<li>2 &ndash;&nbsp;Doug Harvey (1947-61)</li>\r\n\t<li>3 &ndash;&nbsp;Butch Bouchard (1941-56)</li>\r\n\t<li>4 &ndash;&nbsp;Jean Beliveau (1950-71)</li>\r\n\t<li>5 &ndash;&nbsp;Bernie&nbsp;Geoffrion (1950-64)</li>\r\n\t<li>5 &ndash;&nbsp;Guy Lapointe (1968-82)</li>\r\n\t<li>7 &ndash;&nbsp;Howie Morenz (1923-37)</li>\r\n\t<li>9 &ndash;&nbsp;Maurice Richard (1942-60)</li>\r\n\t<li>10 &ndash;&nbsp;Guy Lafleur (1971-84)</li>\r\n\t<li>12 &ndash;&nbsp;Dickie Moore (1951-63)</li>\r\n\t<li>12 &ndash;&nbsp;Yvan Cournoyer (1963-79)</li>\r\n\t<li>16 &ndash;&nbsp;Elmer Lach (1940-54)</li>\r\n\t<li>16 &ndash;&nbsp;Henri Richard (1955-75)</li>\r\n\t<li>18 &ndash;&nbsp;Serge Savard (1966-81)</li>\r\n\t<li>19 &ndash;&nbsp;Larry Robinson (1972-89)</li>\r\n\t<li>23 &ndash;&nbsp;Bob Gainey (1973-89)</li>\r\n\t<li>29 &ndash;&nbsp;Ken Dryden (1970-79)</li>\r\n\t<li>33 &ndash;&nbsp;Patrick Roy (1984-95)</li>\r\n</ul>\r\n
    ##   data.teamAbbrev  data.teamFullName total
    ## 1             MTL Montréal Canadiens     1

``` r
# I choose Franchise full name="New York Islanders"
head(wrapfnc("record","franchise","detail","mostRecentTeamId","New York Islanders"))
```

    ##   data.id data.active
    ## 1      22        TRUE
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   data.captainHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Anders Lee: 2018-19 &ndash;&nbsp;Present</li>\r\n\t<li>John Tavares: 2013-14 &ndash;&nbsp;2017-18</li>\r\n\t<li>Mark Streit: 2011-12 &ndash;&nbsp;2012-13</li>\r\n\t<li>Doug Weight: 2009-10 &ndash;&nbsp;2010-11</li>\r\n\t<li>Bill Guerin and (No Captain): 2008-09</li>\r\n\t<li>Bill Guerin: 2007-08</li>\r\n\t<li>Alexei Yashin: 2005-06 &ndash;&nbsp;2006-07</li>\r\n\t<li>Michael Peca: 2001-02 &ndash;&nbsp;2003-04</li>\r\n\t<li>Kenny Jonsson: 1999-00 &ndash;&nbsp;2000-01</li>\r\n\t<li>Trevor Linden: 1998-99</li>\r\n\t<li>Bryan McCabe and Trevor Linden: 1997-98</li>\r\n\t<li>(No Captain): 1996-97</li>\r\n\t<li>Patrick Flatley: 1992-93 &ndash;&nbsp;1995-96</li>\r\n\t<li>Brent Sutter and Patrick Flatley: 1991-92</li>\r\n\t<li>Brent Sutter: 1987-88 &ndash;&nbsp;1990-91</li>\r\n\t<li>Denis Potvin: 1979-80 &ndash;&nbsp;1986-87</li>\r\n\t<li>Clark Gillies: 1977-78 &ndash;&nbsp;1978-79</li>\r\n\t<li>Ed Westfall and Clark Gillies: 1976-77</li>\r\n\t<li>Ed Westfall: 1972-73 &ndash;&nbsp;1975-76</li>\r\n</ul>\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    data.coachingHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Barry Trotz: Oct. 4, 2018 &ndash; Present</li>\r\n\t<li>Doug Weight: Jan. 19, 2017 &ndash; April 7, 2018</li>\r\n\t<li>Jack Capuano: Nov. 17, 2010 &ndash; Jan. 16, 2017</li>\r\n\t<li>Scott Gordon: Oct. 10, 2008 &ndash; Nov. 13, 2010</li>\r\n\t<li>Ted Nolan: Nov. 3, 2007 &ndash; April 4, 2008</li>\r\n\t<li>Al Arbour: Nov. 1, 2007</li>\r\n\t<li>Ted Nolan: Oct. 5, 2006 &ndash; Oct. 27, 2007</li>\r\n\t<li>Brad Shaw: Jan. 12&nbsp;&ndash; April 18, 2006</li>\r\n\t<li>Steve Stirling: Oct. 9, 2003 &ndash; Jan. 10, 2006</li>\r\n\t<li>Peter Laviolette: Oct. 5, 2001 &ndash; April 17, 2003</li>\r\n\t<li>Lorne Henning: March 5&nbsp;&ndash; April 7, 2001</li>\r\n\t<li>Butch Goring: Oct. 2, 1999 &ndash; March 3, 2001</li>\r\n\t<li>Bill Stewart: Jan. 21&nbsp;&ndash; April 17, 1999</li>\r\n\t<li>Mike Milbury: March 12, 1998 &ndash; Jan. 20, 1999</li>\r\n\t<li>Rick Bowness: Jan. 24, 1997 &ndash; March 10, 1998</li>\r\n\t<li>Mike Milbury: Oct. 7, 1995 &ndash; Jan. 22, 1997</li>\r\n\t<li>Lorne Henning: Jan. 21&nbsp;&ndash; May 2, 1995</li>\r\n\t<li>Al Arbour: Dec. 9, 1988 &ndash; April 24, 1994</li>\r\n\t<li>Terry Simpson: Oct. 9, 1986 &ndash; Dec. 6, 1988</li>\r\n\t<li>Al Arbour: Oct. 10, 1973 &ndash; April 12, 1986</li>\r\n\t<li>Earl Ingarfield: Jan. 31&nbsp;&ndash; April 1, 1973</li>\r\n\t<li>Phil Goyette: Oct. 7, 1972 &ndash; Jan. 26, 1973</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ##      data.dateAwarded
    ## 1 1972-06-06T00:00:00
    ##                                       data.directoryUrl data.firstSeasonId
    ## 1 https://www.nhl.com/islanders/team/business-directory           19721973
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               data.generalManagerHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Lou Lamoriello: June 5, 2018 &ndash; Present</li>\r\n\t<li>Garth Snow: July 18, 2006 &ndash; June 5, 2018</li>\r\n\t<li>Neil Smith: June 6&nbsp;&ndash; July 18, 2006</li>\r\n\t<li>Mike Milbury: Dec. 12, 1995 &ndash; June 6, 2006</li>\r\n\t<li>Darcy Regier: Dec. 2-12, 1995</li>\r\n\t<li>Don Maloney: Aug. 17, 1992 &ndash; Dec. 2, 1995</li>\r\n\t<li>Bill Torrey: Feb. 14, 1972 &ndash; Aug. 17, 1992</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ##                                                              data.heroImageUrl
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/NYI/Barzal.jpg
    ##   data.mostRecentTeamId
    ## 1                     2
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                              data.retiredNumbersSummary
    ## 1 <ul class="striped-list">\r\n\t<li>5 &ndash;&nbsp;Denis Potvin (1973-88)</li>\r\n\t<li>9 &ndash;&nbsp;Clark Gillies (1974-86)</li>\r\n\t<li>19 &ndash;&nbsp;Bryan Trottier (1975-90)</li>\r\n\t<li>22 &ndash;&nbsp;Mike Bossy (1977-87)</li>\r\n\t<li>23 &ndash;&nbsp;Bobby Nystrom (1972-86)</li>\r\n\t<li>27 &ndash;&nbsp;John Tonelli (1978-86)</li>\r\n\t<li>31 &ndash;&nbsp;Billy Smith (1972-89)</li>\r\n\t<li>91 &ndash;&nbsp;Butch Goring (1980-84)</li>\r\n</ul>\r\n
    ##   data.teamAbbrev  data.teamFullName total
    ## 1             NYI New York Islanders     1

## Team Stat modifier

``` r
# I choose all ID
head(wrapfnc("stat","teams"))
```

    ## # A tibble: 6 x 66
    ##   copyright teams.id teams.name teams.link teams.abbreviat~ teams.teamName
    ##   <fct>        <int> <chr>      <chr>      <chr>            <chr>         
    ## 1 NHL and ~        1 New Jerse~ /api/v1/t~ NJD              Devils        
    ## 2 NHL and ~        1 New Jerse~ /api/v1/t~ NJD              Devils        
    ## 3 NHL and ~        2 New York ~ /api/v1/t~ NYI              Islanders     
    ## 4 NHL and ~        2 New York ~ /api/v1/t~ NYI              Islanders     
    ## 5 NHL and ~        3 New York ~ /api/v1/t~ NYR              Rangers       
    ## 6 NHL and ~        3 New York ~ /api/v1/t~ NYR              Rangers       
    ## # ... with 60 more variables: teams.locationName <chr>,
    ## #   teams.firstYearOfPlay <chr>, stat.gamesPlayed <int>, stat.wins <chr>,
    ## #   stat.losses <chr>, stat.ot <chr>, stat.pts <chr>, stat.ptPctg <chr>,
    ## #   stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>,
    ## #   stat.evGGARatio <chr>, stat.powerPlayPercentage <chr>,
    ## #   stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>,
    ## #   stat.powerPlayOpportunities <chr>, stat.penaltyKillPercentage <chr>,
    ## #   stat.shotsPerGame <chr>, stat.shotsAllowed <chr>,
    ## #   stat.winScoreFirst <chr>, stat.winOppScoreFirst <chr>,
    ## #   stat.winLeadFirstPer <chr>, stat.winLeadSecondPer <chr>,
    ## #   stat.winOutshootOpp <chr>, stat.winOutshotByOpp <chr>,
    ## #   stat.faceOffsTaken <chr>, stat.faceOffsWon <chr>,
    ## #   stat.faceOffsLost <chr>, stat.faceOffWinPercentage <chr>,
    ## #   stat.shootingPctg <dbl>, stat.savePctg <dbl>,
    ## #   stat.penaltyKillOpportunities <chr>, stat.savePctRank <chr>,
    ## #   stat.shootingPctRank <chr>, team.id <int>, team.name <chr>,
    ## #   team.link <chr>, type.displayName <chr>, type.gameType.id <chr>,
    ## #   type.gameType.description <chr>, type.gameType.postseason <lgl>,
    ## #   teams.shortName <chr>, teams.officialSiteUrl <chr>,
    ## #   teams.franchiseId <int>, teams.active <lgl>, teams.venue.name <chr>,
    ## #   teams.venue.link <chr>, teams.venue.city <chr>, teams.venue.id <int>,
    ## #   teams.venue.timeZone.id <chr>, teams.venue.timeZone.offset <int>,
    ## #   teams.venue.timeZone.tz <chr>, teams.division.id <int>,
    ## #   teams.division.name <chr>, teams.division.link <chr>,
    ## #   teams.conference.id <int>, teams.conference.name <chr>,
    ## #   teams.conference.link <chr>, teams.franchise.franchiseId <int>,
    ## #   teams.franchise.teamName <chr>, teams.franchise.link <chr>

``` r
# I choose ID=1
head(wrapfnc("stat","teams",1))
```

    ## # A tibble: 2 x 65
    ##   copyright teams.id teams.name teams.link teams.abbreviat~ teams.teamName
    ##   <fct>        <int> <chr>      <chr>      <chr>            <chr>         
    ## 1 NHL and ~        1 New Jerse~ /api/v1/t~ NJD              Devils        
    ## 2 NHL and ~        1 New Jerse~ /api/v1/t~ NJD              Devils        
    ## # ... with 59 more variables: teams.locationName <chr>,
    ## #   teams.firstYearOfPlay <chr>, stat.gamesPlayed <int>, stat.wins <chr>,
    ## #   stat.losses <chr>, stat.ot <chr>, stat.pts <chr>, stat.ptPctg <chr>,
    ## #   stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>,
    ## #   stat.evGGARatio <chr>, stat.powerPlayPercentage <chr>,
    ## #   stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>,
    ## #   stat.powerPlayOpportunities <chr>, stat.penaltyKillPercentage <chr>,
    ## #   stat.shotsPerGame <chr>, stat.shotsAllowed <chr>,
    ## #   stat.winScoreFirst <chr>, stat.winOppScoreFirst <chr>,
    ## #   stat.winLeadFirstPer <chr>, stat.winLeadSecondPer <chr>,
    ## #   stat.winOutshootOpp <chr>, stat.winOutshotByOpp <chr>,
    ## #   stat.faceOffsTaken <chr>, stat.faceOffsWon <chr>,
    ## #   stat.faceOffsLost <chr>, stat.faceOffWinPercentage <chr>,
    ## #   stat.shootingPctg <dbl>, stat.savePctg <dbl>,
    ## #   stat.penaltyKillOpportunities <chr>, stat.savePctRank <chr>,
    ## #   stat.shootingPctRank <chr>, team.id <int>, team.name <chr>,
    ## #   team.link <chr>, type.displayName <chr>, type.gameType.id <chr>,
    ## #   type.gameType.description <chr>, type.gameType.postseason <lgl>,
    ## #   teams.shortName <chr>, teams.officialSiteUrl <chr>,
    ## #   teams.franchiseId <int>, teams.active <lgl>, teams.venue.name <chr>,
    ## #   teams.venue.link <chr>, teams.venue.city <chr>,
    ## #   teams.venue.timeZone.id <chr>, teams.venue.timeZone.offset <int>,
    ## #   teams.venue.timeZone.tz <chr>, teams.division.id <int>,
    ## #   teams.division.name <chr>, teams.division.link <chr>,
    ## #   teams.conference.id <int>, teams.conference.name <chr>,
    ## #   teams.conference.link <chr>, teams.franchise.franchiseId <int>,
    ## #   teams.franchise.teamName <chr>, teams.franchise.link <chr>

``` r
# I choose Franchise full name="New Jersey Devils"
head(wrapfnc("record","franchise","detail","mostRecentTeamId","New Jersey Devils"))
```

    ##   data.id data.active
    ## 1      23        TRUE
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  data.captainHistory
    ## 1 <ul class="striped-list">\r\n\t<li>(No Captain) and Nico Hischier: 2020-21</li>\r\n\t<li>Andy Greene and (No Captain): 2019-20</li>\r\n\t<li>Andy Greene: 2015-16 &ndash;&nbsp;2018-19</li>\r\n\t<li>Bryce Salvador: 2012-13 &ndash;&nbsp;2014-15</li>\r\n\t<li>Zach Parise: 2011-12</li>\r\n\t<li>Jamie Langenbrunner: 2008-09 &ndash;&nbsp;2010-11</li>\r\n\t<li>Patrik Elias and Jamie Langenbrunner: 2007-08</li>\r\n\t<li>Patrik Elias: 2006-07</li>\r\n\t<li>(No Captain): 2005-06</li>\r\n\t<li>Scott Stevens and Scott Niedermayer: 2003-04</li>\r\n\t<li>Scott Stevens: 1992-93 &ndash;&nbsp;2002-03</li>\r\n\t<li>Bruce Driver: 1991-92</li>\r\n\t<li>Kirk Muller: 1987-88 &ndash;&nbsp;1990-91</li>\r\n\t<li>Mel Bridgman: 1984-85 &ndash;&nbsp;1986-87</li>\r\n\t<li>Don Lever and Mel Bridgman: 1983-84</li>\r\n\t<li>Don Lever: 1982-83</li>\r\n\t<li>Lanny McDonald and Rob Ramage: 1981-82</li>\r\n\t<li>Lanny McDonald: 1980-81</li>\r\n\t<li>Mike Christie, Rene Robert and Lanny McDonald: 1979-80</li>\r\n\t<li>Gary Croteau: 1978-79</li>\r\n\t<li>Wilf Paiement: 1977-78</li>\r\n\t<li>Simon Nolet: 1974-75 &ndash;&nbsp;1976-77</li>\r\n</ul>\r\n
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       data.coachingHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Lindy Ruff: Present</li>\r\n\t<li>Alain Nasreddine: Dec. 3, 2019 &ndash; March 10, 2020</li>\r\n\t<li>John Hynes: Oct. 9, 2015 &ndash; Dec. 2, 2019</li>\r\n\t<li>Peter DeBoer: Oct. 8, 2011 &ndash; Dec. 23, 2014</li>\r\n\t<li>Jacques Lemaire: Dec. 23, 2010 &ndash; April 10, 2011</li>\r\n\t<li>John MacLean: Oct. 8&nbsp;&ndash; Dec. 21, 2010</li>\r\n\t<li>Jacques Lemaire: Oct. 3, 2009 &ndash; April 22, 2010</li>\r\n\t<li>Brent Sutter: Oct. 4, 2007 &ndash; April 28, 2009</li>\r\n\t<li>Lou Lamoriello: April 3&nbsp;&ndash; May 5, 2007</li>\r\n\t<li>Claude Julien: Oct. 6, 2006 &ndash; April 1, 2007</li>\r\n\t<li>Lou Lamoriello: Dec. 20, 2005 &ndash; May 14, 2006</li>\r\n\t<li>Larry Robinson: Oct. 5&nbsp;&ndash; Dec. 17, 2005</li>\r\n\t<li>Pat Burns: Oct. 10, 2002 &ndash; April 17, 2004</li>\r\n\t<li>Kevin Constantine: Jan. 29&nbsp;&ndash; April 27, 2002</li>\r\n\t<li>Larry Robinson: March 24, 2000 &ndash; Jan. 26, 2002</li>\r\n\t<li>Robbie Ftorek: Oct. 10, 1998 &ndash; March 21, 2000</li>\r\n\t<li>Jacques Lemaire: Oct. 6, 1993 &ndash; May 2, 1998</li>\r\n\t<li>Herb Brooks: Oct. 6, 1992 &ndash; April 26, 1993</li>\r\n\t<li>Tom McVie: March 5, 1991 &ndash; May 1, 1992</li>\r\n\t<li>John Cunniff: Nov. 8, 1989 &ndash; March 3, 1991</li>\r\n\t<li>Jim Schoenfeld: Jan. 28, 1988 &ndash; Nov. 4, 1989</li>\r\n\t<li>Doug Carpenter: Oct. 12, 1984 &ndash; Jan. 25, 1988</li>\r\n\t<li>Tom McVie: Nov. 23, 1983 &ndash; April 1, 1984</li>\r\n\t<li>Bill MacMillan: Oct. 5, 1982 &ndash; Nov. 19, 1983</li>\r\n\t<li>Marshall Johnston: Nov. 30, 1981 &ndash; April 3, 1982</li>\r\n\t<li>Bert Marshall: Oct. 6&nbsp;&ndash; Nov. 28, 1981</li>\r\n\t<li>Bill MacMillan: Oct. 11, 1980 &ndash; April 4, 1981</li>\r\n\t<li>Don Cherry: Oct. 11, 1979 &ndash; April 6, 1980</li>\r\n\t<li>Aldo Guidolin: Nov. 25, 1978 &ndash; April 8, 1979</li>\r\n\t<li>Pat Kelly: Oct. 14, 1977 &ndash; Nov. 23, 1978</li>\r\n\t<li>Johnny Wilson: Oct. 5, 1976 &ndash; April 3, 1977</li>\r\n\t<li>Eddie Bush: Jan. 28&nbsp;&ndash; April 4, 1976</li>\r\n\t<li>Sid Abel: Jan. 21-25, 1976</li>\r\n\t<li>Bep Guidolin: Oct. 9, 1974 &ndash; Jan. 17, 1976</li>\r\n\t<li>* <em>Date range indicates first and last games coached during tenure (regular season or playoffs)</em></li>\r\n</ul>\r\n
    ##      data.dateAwarded                               data.directoryUrl
    ## 1 1974-06-11T00:00:00 https://www.nhl.com/devils/team/staff-directory
    ##   data.firstSeasonId
    ## 1           19741975
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            data.generalManagerHistory
    ## 1 <ul class="striped-list">\r\n\t<li>Tom Fitzgerald: Jan. 12, 2020 &ndash; Present</li>\r\n\t<li>Ray Shero: May 4, 2015 &ndash; Jan. 12, 2020</li>\r\n\t<li>Lou Lamoriello: Sept. 10, 1987 &ndash; May 4, 2015</li>\r\n\t<li>Max McNab: Nov. 22, 1983 &ndash; Sept. 10, 1987</li>\r\n\t<li>Bill MacMillan: May 4, 1981 &ndash; Nov. 22, 1983</li>\r\n\t<li>Ray Miron: Aug. 23, 1976 &ndash; May 1, 1981</li>\r\n\t<li>Baz Bastien: Feb. 14&nbsp;&ndash; April 5, 1976</li>\r\n\t<li>Sid Abel: June 1, 1973 &ndash; Feb. 13, 1976</li>\r\n\t<li>* <em>Date range indicates first and last days of tenure</em></li>\r\n</ul>\r\n
    ##                                                              data.heroImageUrl
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/NJD/Subban.jpg
    ##   data.mostRecentTeamId
    ## 1                     1
    ##                                                                                                                                                                                                                                                                                        data.retiredNumbersSummary
    ## 1 <ul class="striped-list">\r\n\t<li>3 &ndash;&nbsp;Ken Daneyko (1982-03)</li>\r\n\t<li>4 &ndash;&nbsp;Scott Stevens (1991-05)</li>\r\n\t<li>26 &ndash;&nbsp;Patrik Elias (1996-16)</li>\r\n\t<li>27 &ndash;&nbsp;Scott Niedermayer (1991-04)</li>\r\n\t<li>30 &ndash;&nbsp;Martin Brodeur (1992-14)</li>\r\n</ul>\r\n
    ##   data.teamAbbrev data.teamFullName total
    ## 1             NJD New Jersey Devils     1

# Getting basic ideas

## Choose two franchises to compare with

``` r
# Look for interesting results from 'team-total' dataset. 
a <- wrapfnc("record","franchise","team-totals") %>% filter(data.gameTypeId==2 & data.gamesPlayed >2000) %>% 
  mutate(Winchance=data.wins/data.gamesPlayed) %>% select(data.franchiseId,data.gamesPlayed, data.wins, Winchance)
knitr::kable(a)
```

<table>
<thead>
<tr>
<th style="text-align:right;">
data.franchiseId
</th>
<th style="text-align:right;">
data.gamesPlayed
</th>
<th style="text-align:right;">
data.wins
</th>
<th style="text-align:right;">
Winchance
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
2993
</td>
<td style="text-align:right;">
1394
</td>
<td style="text-align:right;">
0.4657534
</td>
</tr>
<tr>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
3788
</td>
<td style="text-align:right;">
1688
</td>
<td style="text-align:right;">
0.4456177
</td>
</tr>
<tr>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
6560
</td>
<td style="text-align:right;">
2883
</td>
<td style="text-align:right;">
0.4394817
</td>
</tr>
<tr>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
4171
</td>
<td style="text-align:right;">
2079
</td>
<td style="text-align:right;">
0.4984416
</td>
</tr>
<tr>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
4171
</td>
<td style="text-align:right;">
1903
</td>
<td style="text-align:right;">
0.4562455
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6626
</td>
<td style="text-align:right;">
3241
</td>
<td style="text-align:right;">
0.4891337
</td>
</tr>
<tr>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
3945
</td>
<td style="text-align:right;">
1805
</td>
<td style="text-align:right;">
0.4575412
</td>
</tr>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
6787
</td>
<td style="text-align:right;">
3473
</td>
<td style="text-align:right;">
0.5117136
</td>
</tr>
<tr>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
2195
</td>
<td style="text-align:right;">
971
</td>
<td style="text-align:right;">
0.4423690
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
6516
</td>
<td style="text-align:right;">
2873
</td>
<td style="text-align:right;">
0.4409147
</td>
</tr>
<tr>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
2109
</td>
<td style="text-align:right;">
889
</td>
<td style="text-align:right;">
0.4215268
</td>
</tr>
<tr>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
2194
</td>
<td style="text-align:right;">
985
</td>
<td style="text-align:right;">
0.4489517
</td>
</tr>
<tr>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
3633
</td>
<td style="text-align:right;">
1700
</td>
<td style="text-align:right;">
0.4679328
</td>
</tr>
<tr>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
6560
</td>
<td style="text-align:right;">
2812
</td>
<td style="text-align:right;">
0.4286585
</td>
</tr>
<tr>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
6293
</td>
<td style="text-align:right;">
2891
</td>
<td style="text-align:right;">
0.4593993
</td>
</tr>
<tr>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
4173
</td>
<td style="text-align:right;">
1929
</td>
<td style="text-align:right;">
0.4622574
</td>
</tr>
<tr>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
3154
</td>
<td style="text-align:right;">
1497
</td>
<td style="text-align:right;">
0.4746354
</td>
</tr>
<tr>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
3235
</td>
<td style="text-align:right;">
1469
</td>
<td style="text-align:right;">
0.4540958
</td>
</tr>
<tr>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
3945
</td>
<td style="text-align:right;">
1649
</td>
<td style="text-align:right;">
0.4179975
</td>
</tr>
<tr>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
2111
</td>
<td style="text-align:right;">
990
</td>
<td style="text-align:right;">
0.4689721
</td>
</tr>
<tr>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
2109
</td>
<td style="text-align:right;">
1084
</td>
<td style="text-align:right;">
0.5139877
</td>
</tr>
<tr>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
4172
</td>
<td style="text-align:right;">
1754
</td>
<td style="text-align:right;">
0.4204219
</td>
</tr>
<tr>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
2274
</td>
<td style="text-align:right;">
1070
</td>
<td style="text-align:right;">
0.4705365
</td>
</tr>
<tr>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
2062
</td>
<td style="text-align:right;">
758
</td>
<td style="text-align:right;">
0.3676043
</td>
</tr>
</tbody>
</table>

ID=1 had higher chance of win than ID=20 had. Let’s find the factors
which affect team winning!

## Goals by skater position

``` r
skr <- wrapfnc("record","franchise","skater-records")
p <- ggplot(skr,aes(x=data.positionCode,y=data.goals,fill=data.positionCode))
p+geom_col()+scale_fill_brewer(palette = "Set2")+labs(x="Position",y="Goals",color="Position",title="< Goals by Position >")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Center position skater scored most goals and then right winger did. That
makes sense!

## Assists by skater position

``` r
a <- ggplot(skr,aes(x=data.positionCode, y=data.assists, fill=data.positionCode))
a+geom_col()+ scale_fill_brewer(palette = "PRGn")+labs(x="Position",y="Goals",color="Position",title="< Assists by Position >")
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Center players assisted most and then Defenders did. I guessed wingers
assisted most but the data tells different story. Interesting!!

# FACTORS WHICH INFLUENCE TEAM WINNING

## How skater assists affect winning?

``` r
# Filter datasets and create new variables
skid1 <- wrapfnc("record","franchise", "skater-records","franchiseId",1)
skid20 <- wrapfnc("record","franchise", "skater-records","franchiseId",20)

# Combine two datasets
cosk <- rbind(skid1,skid20)
cosk$data.franchiseId <- as.character(cosk$data.franchiseId)

# Summarise skaters assists records
avgassi <- c(ID.1=mean(skid1$data.assists),ID.20=mean(skid20$data.assists))
knitr::kable(avgassi,col.names = "Avg Assists")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Avg Assists
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ID.1
</td>
<td style="text-align:right;">
41.49625
</td>
</tr>
<tr>
<td style="text-align:left;">
ID.20
</td>
<td style="text-align:right;">
34.80696
</td>
</tr>
</tbody>
</table>

We can see Franchise ID=1 had more average assists from the data.

``` r
# Create a graph to see the relationship between assists and goals
sk <- ggplot(cosk, aes(x=data.assists, y=data.goals))
sk+geom_jitter(aes(color=data.franchiseId))+labs(x="Assists",y="Goals",color="Franchise ID",title="< Assists and Goals >")
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

The franchise ID=1 had more average assists and higher winning chance
than ID=20 did. This graph also tells us the relationship between
assists and goals is linear. So, we can say **more assists leads higher
chance of winning!**

## How center position skater’s penalty time affect goal?

``` r
# Summarise center position skater's penalty minutes records
center <- cosk %>% filter(data.positionCode== "C")
centerpt <- ggplot(center, aes(x=data.penaltyMinutes, y=data.goals))
centerpt + geom_line(aes(color=data.franchiseId)) + geom_smooth() +
  labs(x="Penalty Minutes", y="Goals",color="Franchise ID", title="< center skater Penalty minute and Goals >")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

The skaters who are in a center position scored more goals as they had
more penalty minutes. What?? Interesting!

``` r
ttotal <- wrapfnc("record","franchise","team-totals")
w <- ggplot(ttotal,aes(data.penaltyMinutes,data.wins))
g <- ggplot(ttotal,aes(data.penaltyMinutes,data.goalsFor))
g+geom_quantile()+labs(x="Penalty Minutes", y="Goals", title="< Penalty minutes and Goals >")
```

    ## Smoothing formula not specified. Using: y ~ x

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
w+geom_quantile()+labs(x="Penalty Minutes", y="Wins", title="< Penalty minutes and Wins >")
```

    ## Smoothing formula not specified. Using: y ~ x

![](README_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

What a surprising result!! I thought the penalty minutes would affects
goals and winnings in negative ways, but the graphs tell us totally
opposite story.

## Is playing at Home really an advantage?

``` r
hwratio <- ttotal %>% filter(data.gameTypeId==2) %>% mutate(HomeWin.ratio=data.homeWins/data.wins) %>%
  select(data.teamName,data.homeWins, data.wins, HomeWin.ratio)
knitr::kable(hwratio)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
data.teamName
</th>
<th style="text-align:right;">
data.homeWins
</th>
<th style="text-align:right;">
data.wins
</th>
<th style="text-align:right;">
HomeWin.ratio
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
New Jersey Devils
</td>
<td style="text-align:right;">
790
</td>
<td style="text-align:right;">
1394
</td>
<td style="text-align:right;">
0.5667145
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
963
</td>
<td style="text-align:right;">
1688
</td>
<td style="text-align:right;">
0.5704976
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Rangers
</td>
<td style="text-align:right;">
1614
</td>
<td style="text-align:right;">
2883
</td>
<td style="text-align:right;">
0.5598335
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Flyers
</td>
<td style="text-align:right;">
1216
</td>
<td style="text-align:right;">
2079
</td>
<td style="text-align:right;">
0.5848966
</td>
</tr>
<tr>
<td style="text-align:left;">
Pittsburgh Penguins
</td>
<td style="text-align:right;">
1138
</td>
<td style="text-align:right;">
1903
</td>
<td style="text-align:right;">
0.5980032
</td>
</tr>
<tr>
<td style="text-align:left;">
Boston Bruins
</td>
<td style="text-align:right;">
1885
</td>
<td style="text-align:right;">
3241
</td>
<td style="text-align:right;">
0.5816106
</td>
</tr>
<tr>
<td style="text-align:left;">
Buffalo Sabres
</td>
<td style="text-align:right;">
1053
</td>
<td style="text-align:right;">
1805
</td>
<td style="text-align:right;">
0.5833795
</td>
</tr>
<tr>
<td style="text-align:left;">
Montréal Canadiens
</td>
<td style="text-align:right;">
2038
</td>
<td style="text-align:right;">
3473
</td>
<td style="text-align:right;">
0.5868126
</td>
</tr>
<tr>
<td style="text-align:left;">
Ottawa Senators
</td>
<td style="text-align:right;">
533
</td>
<td style="text-align:right;">
971
</td>
<td style="text-align:right;">
0.5489186
</td>
</tr>
<tr>
<td style="text-align:left;">
Toronto Maple Leafs
</td>
<td style="text-align:right;">
1702
</td>
<td style="text-align:right;">
2873
</td>
<td style="text-align:right;">
0.5924121
</td>
</tr>
<tr>
<td style="text-align:left;">
Atlanta Thrashers
</td>
<td style="text-align:right;">
183
</td>
<td style="text-align:right;">
342
</td>
<td style="text-align:right;">
0.5350877
</td>
</tr>
<tr>
<td style="text-align:left;">
Carolina Hurricanes
</td>
<td style="text-align:right;">
453
</td>
<td style="text-align:right;">
827
</td>
<td style="text-align:right;">
0.5477630
</td>
</tr>
<tr>
<td style="text-align:left;">
Florida Panthers
</td>
<td style="text-align:right;">
485
</td>
<td style="text-align:right;">
889
</td>
<td style="text-align:right;">
0.5455568
</td>
</tr>
<tr>
<td style="text-align:left;">
Tampa Bay Lightning
</td>
<td style="text-align:right;">
559
</td>
<td style="text-align:right;">
985
</td>
<td style="text-align:right;">
0.5675127
</td>
</tr>
<tr>
<td style="text-align:left;">
Washington Capitals
</td>
<td style="text-align:right;">
959
</td>
<td style="text-align:right;">
1700
</td>
<td style="text-align:right;">
0.5641176
</td>
</tr>
<tr>
<td style="text-align:left;">
Chicago Blackhawks
</td>
<td style="text-align:right;">
1655
</td>
<td style="text-align:right;">
2812
</td>
<td style="text-align:right;">
0.5885491
</td>
</tr>
<tr>
<td style="text-align:left;">
Detroit Red Wings
</td>
<td style="text-align:right;">
1741
</td>
<td style="text-align:right;">
2891
</td>
<td style="text-align:right;">
0.6022138
</td>
</tr>
<tr>
<td style="text-align:left;">
Nashville Predators
</td>
<td style="text-align:right;">
477
</td>
<td style="text-align:right;">
852
</td>
<td style="text-align:right;">
0.5598592
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Blues
</td>
<td style="text-align:right;">
1122
</td>
<td style="text-align:right;">
1929
</td>
<td style="text-align:right;">
0.5816485
</td>
</tr>
<tr>
<td style="text-align:left;">
Calgary Flames
</td>
<td style="text-align:right;">
863
</td>
<td style="text-align:right;">
1497
</td>
<td style="text-align:right;">
0.5764863
</td>
</tr>
<tr>
<td style="text-align:left;">
Colorado Avalanche
</td>
<td style="text-align:right;">
543
</td>
<td style="text-align:right;">
1007
</td>
<td style="text-align:right;">
0.5392254
</td>
</tr>
<tr>
<td style="text-align:left;">
Edmonton Oilers
</td>
<td style="text-align:right;">
830
</td>
<td style="text-align:right;">
1469
</td>
<td style="text-align:right;">
0.5650102
</td>
</tr>
<tr>
<td style="text-align:left;">
Vancouver Canucks
</td>
<td style="text-align:right;">
943
</td>
<td style="text-align:right;">
1649
</td>
<td style="text-align:right;">
0.5718617
</td>
</tr>
<tr>
<td style="text-align:left;">
Anaheim Ducks
</td>
<td style="text-align:right;">
557
</td>
<td style="text-align:right;">
990
</td>
<td style="text-align:right;">
0.5626263
</td>
</tr>
<tr>
<td style="text-align:left;">
Dallas Stars
</td>
<td style="text-align:right;">
594
</td>
<td style="text-align:right;">
1084
</td>
<td style="text-align:right;">
0.5479705
</td>
</tr>
<tr>
<td style="text-align:left;">
Los Angeles Kings
</td>
<td style="text-align:right;">
1027
</td>
<td style="text-align:right;">
1754
</td>
<td style="text-align:right;">
0.5855188
</td>
</tr>
<tr>
<td style="text-align:left;">
Phoenix Coyotes
</td>
<td style="text-align:right;">
340
</td>
<td style="text-align:right;">
615
</td>
<td style="text-align:right;">
0.5528455
</td>
</tr>
<tr>
<td style="text-align:left;">
San Jose Sharks
</td>
<td style="text-align:right;">
589
</td>
<td style="text-align:right;">
1070
</td>
<td style="text-align:right;">
0.5504673
</td>
</tr>
<tr>
<td style="text-align:left;">
Columbus Blue Jackets
</td>
<td style="text-align:right;">
390
</td>
<td style="text-align:right;">
678
</td>
<td style="text-align:right;">
0.5752212
</td>
</tr>
<tr>
<td style="text-align:left;">
Minnesota Wild
</td>
<td style="text-align:right;">
429
</td>
<td style="text-align:right;">
759
</td>
<td style="text-align:right;">
0.5652174
</td>
</tr>
<tr>
<td style="text-align:left;">
Minnesota North Stars
</td>
<td style="text-align:right;">
477
</td>
<td style="text-align:right;">
758
</td>
<td style="text-align:right;">
0.6292876
</td>
</tr>
<tr>
<td style="text-align:left;">
Quebec Nordiques
</td>
<td style="text-align:right;">
300
</td>
<td style="text-align:right;">
497
</td>
<td style="text-align:right;">
0.6036217
</td>
</tr>
<tr>
<td style="text-align:left;">
Winnipeg Jets (1979)
</td>
<td style="text-align:right;">
307
</td>
<td style="text-align:right;">
506
</td>
<td style="text-align:right;">
0.6067194
</td>
</tr>
<tr>
<td style="text-align:left;">
Hartford Whalers
</td>
<td style="text-align:right;">
318
</td>
<td style="text-align:right;">
534
</td>
<td style="text-align:right;">
0.5955056
</td>
</tr>
<tr>
<td style="text-align:left;">
Colorado Rockies
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
113
</td>
<td style="text-align:right;">
0.6902655
</td>
</tr>
<tr>
<td style="text-align:left;">
Ottawa Senators (1917)
</td>
<td style="text-align:right;">
160
</td>
<td style="text-align:right;">
258
</td>
<td style="text-align:right;">
0.6201550
</td>
</tr>
<tr>
<td style="text-align:left;">
Hamilton Tigers
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0.7021277
</td>
</tr>
<tr>
<td style="text-align:left;">
Pittsburgh Pirates
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
0.6119403
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Quakers
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.7500000
</td>
</tr>
<tr>
<td style="text-align:left;">
Detroit Cougars
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:right;">
0.5468750
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Wanderers
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Quebec Bulldogs
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Maroons
</td>
<td style="text-align:right;">
156
</td>
<td style="text-align:right;">
271
</td>
<td style="text-align:right;">
0.5756458
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Americans
</td>
<td style="text-align:right;">
147
</td>
<td style="text-align:right;">
239
</td>
<td style="text-align:right;">
0.6150628
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Eagles
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
0.6363636
</td>
</tr>
<tr>
<td style="text-align:left;">
Oakland Seals
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
66
</td>
<td style="text-align:right;">
0.6666667
</td>
</tr>
<tr>
<td style="text-align:left;">
Atlanta Flames
</td>
<td style="text-align:right;">
161
</td>
<td style="text-align:right;">
268
</td>
<td style="text-align:right;">
0.6007463
</td>
</tr>
<tr>
<td style="text-align:left;">
Kansas City Scouts
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
0.7407407
</td>
</tr>
<tr>
<td style="text-align:left;">
Cleveland Barons
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0.5957447
</td>
</tr>
<tr>
<td style="text-align:left;">
Detroit Falcons
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
0.7352941
</td>
</tr>
<tr>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:right;">
0.6250000
</td>
</tr>
<tr>
<td style="text-align:left;">
Winnipeg Jets
</td>
<td style="text-align:right;">
207
</td>
<td style="text-align:right;">
382
</td>
<td style="text-align:right;">
0.5418848
</td>
</tr>
<tr>
<td style="text-align:left;">
Arizona Coyotes
</td>
<td style="text-align:right;">
116
</td>
<td style="text-align:right;">
214
</td>
<td style="text-align:right;">
0.5420561
</td>
</tr>
<tr>
<td style="text-align:left;">
Vegas Golden Knights
</td>
<td style="text-align:right;">
96
</td>
<td style="text-align:right;">
173
</td>
<td style="text-align:right;">
0.5549133
</td>
</tr>
<tr>
<td style="text-align:left;">
California Golden Seals
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
116
</td>
<td style="text-align:right;">
0.7241379
</td>
</tr>
<tr>
<td style="text-align:left;">
Toronto Arenas
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
0.8333333
</td>
</tr>
<tr>
<td style="text-align:left;">
Toronto St. Patricks
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:right;">
109
</td>
<td style="text-align:right;">
0.6697248
</td>
</tr>
</tbody>
</table>

``` r
r <- ggplot(hwratio,aes(x=HomeWin.ratio))
r+geom_histogram(bins=70,fill="purple")+labs(x="Home wins Ratio", title="< Home Game Winning Chance >")
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

All teams had over 50% of winning chance when they played at home.
Playing at Home really an advantage!!

## Stat

``` r
stat <- wrapfnc("stat","teams")
# PIck odd number rows
stat <- stat[seq(1,62,2),]
```
