Project I
================
Soohee Jung
6/11/2021

-   [Functions](#functions)

# Functions

``` r
library(httr)
library(jsonlite)
library(tidyverse)

frurl <- GET("https://records.nhl.com/site/api/franchise")
frtext <- content(frurl, "text")
frlist <- fromJSON(frtext, flatten=TRUE)
frlist <- as.data.frame(frlist)
frtbl <- tibble(frlist$data.id, frlist$data.teamCommonName)

rcdURL <- function(list,recd,type,id){
  if (missing(recd) & missing(type) & missing(id)){
    rcdurl <- paste0("https://records.nhl.com/site/api/",list)
  }
  else if (missing(type) & missing(id)){
    rcdurl <- paste0("https://records.nhl.com/site/api/",list,"-",recd)
  }
  else {
    if (is.numeric(id)){
      rcdurl <- paste0("https://records.nhl.com/site/api/franchise","-",recd,"?cayenneExp=", type, "=", id)
    }
    else {
      id <- filter(filter(frtbl,frtbl[2]==id)[1])
      rcdurl <- paste0("https://records.nhl.com/site/api/franchise","-",recd,"?cayenneExp=", type, "=", id)
    }
  }
  return(rcdurl)
}

rcddt <- function(list,...){
  rcdNHL <- GET(rcdURL(list,...))
  rcdtext <- content(rcdNHL, "text")
  rcdlist <- fromJSON(rcdtext, flatten=TRUE)
  rcdlist <- as.data.frame(rcdlist)
  return(rcdlist)
}

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
  stattext <- content(statNHL, "text")
  statlist <- fromJSON(stattext, flatten=TRUE)
  statlist <- as.data.frame(statlist)
  return(statlist)
}
```

``` r
# to see variables of each endpoints
str(rcddt("franchise"))
```

    ## No encoding supplied: defaulting to UTF-8.

    ## 'data.frame':    39 obs. of  9 variables:
    ##  $ data.id              : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ data.firstSeasonId   : int  19171918 19171918 19171918 19191920 19171918 19241925 19241925 19251926 19251926 19261927 ...
    ##  $ data.fullName        : chr  "Montréal Canadiens" "Montreal Wanderers" "St. Louis Eagles" "Hamilton Tigers" ...
    ##  $ data.lastSeasonId    : int  NA 19171918 19341935 19241925 NA NA 19371938 19411942 19301931 NA ...
    ##  $ data.mostRecentTeamId: int  8 41 45 37 10 6 43 51 39 3 ...
    ##  $ data.teamAbbrev      : chr  "MTL" "MWN" "SLE" "HAM" ...
    ##  $ data.teamCommonName  : chr  "Canadiens" "Wanderers" "Eagles" "Tigers" ...
    ##  $ data.teamPlaceName   : chr  "Montréal" "Montreal" "St. Louis" "Hamilton" ...
    ##  $ total                : int  39 39 39 39 39 39 39 39 39 39 ...

``` r
str(rcddt("franchise","team-totals"))
```

    ## No encoding supplied: defaulting to UTF-8.

    ## 'data.frame':    105 obs. of  31 variables:
    ##  $ data.id                : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ data.activeFranchise   : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ data.firstSeasonId     : int  19821983 19821983 19721973 19721973 19261927 19261927 19671968 19671968 19671968 19671968 ...
    ##  $ data.franchiseId       : int  23 23 22 22 10 10 16 16 17 17 ...
    ##  $ data.gameTypeId        : int  2 3 2 3 2 3 3 2 2 3 ...
    ##  $ data.gamesPlayed       : int  2993 257 3788 308 6560 518 449 4171 4171 391 ...
    ##  $ data.goalsAgainst      : int  8902 634 11907 895 20020 1447 1332 12255 14049 1131 ...
    ##  $ data.goalsFor          : int  8792 697 12045 982 20041 1404 1335 13690 13874 1190 ...
    ##  $ data.homeLosses        : int  525 53 678 52 1143 104 97 584 683 85 ...
    ##  $ data.homeOvertimeLosses: int  85 0 84 1 76 0 0 93 60 0 ...
    ##  $ data.homeTies          : int  96 NA 170 NA 448 1 NA 193 205 NA ...
    ##  $ data.homeWins          : int  790 74 963 94 1614 137 135 1216 1138 113 ...
    ##  $ data.lastSeasonId      : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ data.losses            : int  1211 120 1587 138 2716 266 218 1452 1734 182 ...
    ##  $ data.overtimeLosses    : int  169 0 166 0 153 0 0 183 151 0 ...
    ##  $ data.penaltyMinutes    : int  44773 4266 57792 5687 86129 8181 9104 76208 66221 6106 ...
    ##  $ data.pointPctg         : num  0.5306 0.0039 0.5133 0.013 0.5127 ...
    ##  $ data.points            : int  3176 2 3889 8 6727 0 4 4798 4340 12 ...
    ##  $ data.roadLosses        : int  686 67 909 86 1573 162 121 868 1051 97 ...
    ##  $ data.roadOvertimeLosses: int  84 0 82 2 77 0 0 90 91 1 ...
    ##  $ data.roadTies          : int  123 NA 177 NA 360 7 NA 264 178 NA ...
    ##  $ data.roadWins          : int  604 63 725 76 1269 107 96 863 765 96 ...
    ##  $ data.shootoutLosses    : int  84 0 70 0 68 0 0 92 54 0 ...
    ##  $ data.shootoutWins      : int  78 0 86 0 79 0 0 53 83 0 ...
    ##  $ data.shutouts          : int  196 25 177 12 408 44 33 248 189 30 ...
    ##  $ data.teamId            : int  1 1 2 2 3 3 4 4 5 5 ...
    ##  $ data.teamName          : chr  "New Jersey Devils" "New Jersey Devils" "New York Islanders" "New York Islanders" ...
    ##  $ data.ties              : int  219 NA 347 NA 808 8 NA 457 383 NA ...
    ##  $ data.triCode           : chr  "NJD" "NJD" "NYI" "NYI" ...
    ##  $ data.wins              : int  1394 137 1688 170 2883 244 231 2079 1903 209 ...
    ##  $ total                  : int  105 105 105 105 105 105 105 105 105 105 ...

``` r
str(rcddt("franchise","season-records"))
```

    ## No encoding supplied: defaulting to UTF-8.

    ## 'data.frame':    39 obs. of  58 variables:
    ##  $ data.id                       : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ data.fewestGoals              : int  174 170 150 173 182 147 157 155 191 147 ...
    ##  $ data.fewestGoalsAgainst       : int  164 190 177 164 188 172 175 131 179 131 ...
    ##  $ data.fewestGoalsAgainstSeasons: chr  "2003-04 (82)" "1975-76 (80)" "1970-71 (78)" "1973-74 (78)" ...
    ##  $ data.fewestGoalsSeasons       : chr  "2010-11 (82)" "1972-73 (78)" "1954-55 (70)" "1967-68 (74)" ...
    ##  $ data.fewestLosses             : int  19 15 17 12 21 13 16 8 21 16 ...
    ##  $ data.fewestLossesSeasons      : chr  "2000-01 (82)" "1978-79 (80)" "1971-72 (78)" "1979-80 (80)" ...
    ##  $ data.fewestPoints             : int  36 30 47 56 38 38 51 65 24 48 ...
    ##  $ data.fewestPointsSeasons      : chr  "1975-76 (80)" "1972-73 (78)" "1965-66 (70)" "2006-07 (82)" ...
    ##  $ data.fewestTies               : int  3 4 4 4 4 5 5 5 4 4 ...
    ##  $ data.fewestTiesSeasons        : chr  "1985-86 (80)" "1983-84 (80)" "2001-02 (82)" "1985-86 (80)" ...
    ##  $ data.fewestWins               : int  12 12 17 17 16 14 16 25 10 20 ...
    ##  $ data.fewestWinsSeasons        : chr  "1975-76 (80)" "1972-73 (78)" "1952-53 (70), 1954-55 (70), 1959-60 (70)" "1969-70 (76)" ...
    ##  $ data.franchiseId              : int  23 22 10 16 17 6 19 1 30 5 ...
    ##  $ data.franchiseName            : chr  "New Jersey Devils" "New York Islanders" "New York Rangers" "Philadelphia Flyers" ...
    ##  $ data.homeLossStreak           : int  10 7 7 8 14 11 7 7 11 7 ...
    ##  $ data.homeLossStreakDates      : chr  "Jan 26 2021 - Mar 13 2021" "Nov 13 1999 - Dec 14 1999, Feb 27 2001 - Mar 29 2001" "Oct 20 1976 - Nov 14 1976, Mar 24 1993 - Apr 14 1993" "Dec 09 2006 - Jan 27 2007" ...
    ##  $ data.homePointStreak          : int  15 23 24 26 20 27 21 34 23 18 ...
    ##  $ data.homePointStreakDates     : chr  "Jan 08 1997 - Mar 15 1997, Dec 14 1999 - Feb 17 2000" "Oct 17 1978 - Jan 20 1979, Jan 02 1982 - Apr 03 1982" "Oct 14 1970 - Jan 31 1971, Oct 24 1995 - Feb 15 1996" "Oct 11 1979 - Feb 03 1980" ...
    ##  $ data.homeWinStreak            : int  11 14 14 20 13 20 12 13 9 13 ...
    ##  $ data.homeWinStreakDates       : chr  "Feb 09 2009 - Mar 20 2009" "Jan 02 1982 - Feb 25 1982" "Dec 19 1939 - Feb 25 1940" "Jan 04 1976 - Apr 03 1976" ...
    ##  $ data.homeWinlessStreak        : int  14 9 10 13 16 11 12 15 17 11 ...
    ##  $ data.homeWinlessStreakDates   : chr  "Feb 12 1976 - Mar 30 1976, Feb 04 1979 - Mar 31 1979" "Mar 02 1999 - Apr 06 1999" "Jan 30 1944 - Mar 19 1944" "Nov 29 2006 - Feb 08 2007" ...
    ##  $ data.lossStreak               : int  14 12 11 9 13 11 14 12 14 10 ...
    ##  $ data.lossStreakDates          : chr  "Dec 30 1975 - Jan 29 1976" "Dec 27 1972 - Jan 16 1973, Nov 22 1988 - Dec 15 1988" "Oct 30 1943 - Nov 27 1943" "Dec 08 2006 - Dec 27 2006" ...
    ##  $ data.mostGameGoals            : int  9 11 12 13 12 14 14 16 11 14 ...
    ##  $ data.mostGameGoalsDates       : chr  "Apr 01 1979 - STL 5 @ CLR 9, Feb 12 1982 - QUE 2 @ CLR 9, Apr 06 1986 - NYI 7 @ NJD 9, Mar 10 1990 - QUE 3 @ NJ"| __truncated__ "Dec 20 1983 - PIT 3 @ NYI 11, Mar 03 1984 - NYI 11 @ TOR 6" "Nov 21 1971 - CGS 1 @ NYR 12" "Mar 22 1984 - PIT 4 @ PHI 13, Oct 18 1984 - VAN 2 @ PHI 13" ...
    ##  $ data.mostGoals                : int  308 385 321 350 367 399 354 387 314 337 ...
    ##  $ data.mostGoalsAgainst         : int  374 347 345 319 394 306 308 295 397 387 ...
    ##  $ data.mostGoalsAgainstSeasons  : chr  "1985-86 (80)" "1972-73 (78)" "1984-85 (80)" "1992-93 (84)" ...
    ##  $ data.mostGoalsSeasons         : chr  "1992-93 (84)" "1981-82 (80)" "1991-92 (80)" "1983-84 (80)" ...
    ##  $ data.mostLosses               : int  56 60 44 48 58 47 51 40 70 52 ...
    ##  $ data.mostLossesSeasons        : chr  "1975-76 (80), 1983-84 (80)" "1972-73 (78)" "1984-85 (80)" "2006-07 (82)" ...
    ##  $ data.mostPenaltyMinutes       : int  2494 1857 2021 2621 2674 2443 2713 1847 1716 2419 ...
    ##  $ data.mostPenaltyMinutesSeasons: chr  "1988-89 (80)" "1986-87 (80)" "1989-90 (80)" "1980-81 (80)" ...
    ##  $ data.mostPoints               : int  111 118 113 118 119 121 113 132 113 105 ...
    ##  $ data.mostPointsSeasons        : chr  "2000-01 (82)" "1981-82 (80)" "2014-15 (82)" "1975-76 (80)" ...
    ##  $ data.mostShutouts             : int  14 11 13 13 10 15 13 22 10 13 ...
    ##  $ data.mostShutoutsSeasons      : chr  "2003-04 (82)" "2018-19 (82)" "1928-29 (44)" "1974-75 (80)" ...
    ##  $ data.mostTies                 : int  21 22 21 24 20 21 21 23 15 22 ...
    ##  $ data.mostTiesSeasons          : chr  "1977-78 (80)" "1974-75 (80)" "1950-51 (70)" "1969-70 (76)" ...
    ##  $ data.mostWins                 : int  51 54 53 53 56 57 53 60 52 49 ...
    ##  $ data.mostWinsSeasons          : chr  "2008-09 (82)" "1981-82 (80)" "2014-15 (82)" "1984-85 (80), 1985-86 (80)" ...
    ##  $ data.pointStreak              : int  16 17 19 35 18 23 14 28 11 16 ...
    ##  $ data.pointStreakDates         : chr  "Dec 26 1999 - Jan 28 2000" "Oct 12 2019 - Nov 23 2019" "Nov 23 1939 - Jan 13 1940" "Oct 14 1979 - Jan 06 1980" ...
    ##  $ data.roadLossStreak           : int  12 15 10 8 18 14 12 10 38 11 ...
    ##  $ data.roadLossStreakDates      : chr  "Oct 19 1983 - Dec 01 1983" "Jan 20 1973 - Mar 31 1973" "Oct 30 1943 - Dec 23 1943, Feb 08 1961 - Mar 15 1961" "Oct 25 1972 - Nov 26 1972, Mar 03 1988 - Mar 29 1988" ...
    ##  $ data.roadPointStreak          : int  10 8 11 16 8 16 10 23 14 11 ...
    ##  $ data.roadPointStreakDates     : chr  "Feb 27 2001 - Apr 07 2001, Jan 30 2007 - Mar 15 2007" "Feb 12 1976 - Mar 14 1976, Nov 12 1978 - Dec 09 1978, Feb 27 1981 - Mar 29 1981, Oct 07 1981 - Nov 11 1981" "Nov 05 1939 - Jan 13 1940" "Oct 20 1979 - Jan 06 1980" ...
    ##  $ data.roadWinStreak            : int  10 8 9 8 8 9 10 8 6 7 ...
    ##  $ data.roadWinStreakDates       : chr  "Feb 27 2001 - Apr 07 2001" "Feb 27 1981 - Mar 29 1981" "Jan 16 2020 - Feb 27 2020" "Dec 22 1982 - Jan 16 1983" ...
    ##  $ data.roadWinlessStreak        : int  32 20 16 19 18 14 23 12 38 18 ...
    ##  $ data.roadWinlessStreakDates   : chr  "Nov 12 1977 - Mar 15 1978" "Nov 03 1972 - Jan 13 1973" "Oct 09 1952 - Dec 20 1952" "Oct 23 1971 - Jan 27 1972" ...
    ##  $ data.winStreak                : int  13 15 10 13 17 14 10 12 11 10 ...
    ##  $ data.winStreakDates           : chr  "Feb 26 2001 - Mar 23 2001" "Jan 21 1982 - Feb 20 1982" "Dec 19 1939 - Jan 13 1940, Jan 19 1973 - Feb 10 1973" "Oct 19 1985 - Nov 17 1985" ...
    ##  $ data.winlessStreak            : int  27 15 21 12 18 20 18 12 21 15 ...
    ##  $ data.winlessStreakDates       : chr  "Feb 12 1976 - Apr 04 1976" "Nov 22 1972 - Dec 21 1972" "Jan 23 1944 - Mar 19 1944" "Feb 24 1999 - Mar 16 1999" ...
    ##  $ total                         : int  39 39 39 39 39 39 39 39 39 39 ...

``` r
str(rcddt("franchise","goalie-records"))
```

    ## No encoding supplied: defaulting to UTF-8.

    ## 'data.frame':    1078 obs. of  30 variables:
    ##  $ data.id                     : int  235 236 237 238 239 240 241 242 243 244 ...
    ##  $ data.activePlayer           : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ data.firstName              : chr  "Don" "Bob" "Tony" "Grant" ...
    ##  $ data.franchiseId            : int  15 28 11 25 16 18 24 18 20 13 ...
    ##  $ data.franchiseName          : chr  "Dallas Stars" "Arizona Coyotes" "Chicago Blackhawks" "Edmonton Oilers" ...
    ##  $ data.gameTypeId             : int  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ data.gamesPlayed            : int  315 281 873 423 489 280 711 347 516 250 ...
    ##  $ data.lastName               : chr  "Beaupre" "Essensa" "Esposito" "Fuhr" ...
    ##  $ data.losses                 : int  125 114 302 117 172 96 293 133 228 140 ...
    ##  $ data.mostGoalsAgainstDates  : chr  "1983-10-07" "1992-12-11, 1992-10-12" "1983-10-15, 1980-11-26" "1984-02-05, 1982-10-12" ...
    ##  $ data.mostGoalsAgainstOneGame: int  10 8 10 9 9 8 8 9 9 11 ...
    ##  $ data.mostSavesDates         : chr  "1987-03-15" "1989-12-29" "1977-02-26" "1986-03-12" ...
    ##  $ data.mostSavesOneGame       : int  52 49 50 49 45 51 52 44 48 55 ...
    ##  $ data.mostShotsAgainstDates  : chr  "1986-03-21" "1989-12-29" "1976-12-12" "1986-03-12" ...
    ##  $ data.mostShotsAgainstOneGame: int  55 50 53 54 50 54 54 48 52 58 ...
    ##  $ data.mostShutoutsOneSeason  : int  1 5 15 4 5 2 6 3 5 4 ...
    ##  $ data.mostShutoutsSeasonIds  : chr  "19841985, 19851986, 19861987" "19911992" "19691970" "19871988" ...
    ##  $ data.mostWinsOneSeason      : int  25 33 38 40 37 36 41 33 38 19 ...
    ##  $ data.mostWinsSeasonIds      : chr  "19851986" "19921993" "19691970" "19871988" ...
    ##  $ data.overtimeLosses         : int  NA NA NA NA NA NA 23 NA NA NA ...
    ##  $ data.playerId               : int  8445381 8446719 8446720 8446991 8447775 8448382 8448535 8448865 8449474 8449550 ...
    ##  $ data.positionCode           : chr  "G" "G" "G" "G" ...
    ##  $ data.rookieGamesPlayed      : int  44 36 63 48 66 30 14 NA 41 56 ...
    ##  $ data.rookieShutouts         : int  0 1 15 0 1 0 0 NA 1 4 ...
    ##  $ data.rookieWins             : int  18 18 38 28 37 16 2 NA 11 16 ...
    ##  $ data.seasons                : int  9 7 15 10 11 6 16 6 11 7 ...
    ##  $ data.shutouts               : int  3 14 74 9 18 5 35 10 20 8 ...
    ##  $ data.ties                   : int  45 32 148 54 58 34 63 52 62 48 ...
    ##  $ data.wins                   : int  126 116 418 226 240 137 301 151 211 58 ...
    ##  $ total                       : int  1078 1078 1078 1078 1078 1078 1078 1078 1078 1078 ...

``` r
str(rcddt("franchise","skater-records","franchiseId",1))
```

    ## No encoding supplied: defaulting to UTF-8.

    ## 'data.frame':    800 obs. of  32 variables:
    ##  $ data.id                         : int  17199 17223 17272 17351 17389 17440 17484 17508 17544 17623 ...
    ##  $ data.activePlayer               : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ data.assists                    : int  0 2 0 0 0 0 0 0 0 0 ...
    ##  $ data.firstName                  : chr  "Reg" "Art" "Dave" "Ossie" ...
    ##  $ data.franchiseId                : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ data.franchiseName              : chr  "Montréal Canadiens" "Montréal Canadiens" "Montréal Canadiens" "Montréal Canadiens" ...
    ##  $ data.gameTypeId                 : int  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ data.gamesPlayed                : int  3 11 3 2 6 2 1 2 1 6 ...
    ##  $ data.goals                      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ data.lastName                   : chr  "Abbott" "Alexandre" "Allison" "Asmundson" ...
    ##  $ data.mostAssistsGameDates       : chr  "1952-10-09, 1952-10-11, 1952-10-12, 1952-10-16, 1952-10-18, 1952-10-19, 1952-10-23, 1952-10-25, 1952-10-29, 195"| __truncated__ "1932-03-08, 1932-03-12" "1983-10-06, 1983-10-08, 1983-10-10, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-20, 1983-10-22, 1983-10-25, 198"| __truncated__ "1937-11-09, 1937-11-11, 1937-11-13, 1937-11-14, 1937-11-18, 1937-11-23, 1937-11-27, 1937-12-02, 1937-12-04, 193"| __truncated__ ...
    ##  $ data.mostAssistsOneGame         : int  0 1 0 0 0 0 0 0 0 0 ...
    ##  $ data.mostAssistsOneSeason       : int  0 2 0 0 0 0 0 0 0 0 ...
    ##  $ data.mostAssistsSeasonIds       : chr  "19521953" "19311932" "19831984" "19371938" ...
    ##  $ data.mostGoalsGameDates         : chr  "1952-10-09, 1952-10-11, 1952-10-12, 1952-10-16, 1952-10-18, 1952-10-19, 1952-10-23, 1952-10-25, 1952-10-29, 195"| __truncated__ "1931-11-12, 1931-11-14, 1931-11-17, 1931-11-21, 1931-11-24, 1931-11-26, 1931-11-29, 1931-12-02, 1931-12-05, 193"| __truncated__ "1983-10-06, 1983-10-08, 1983-10-10, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-20, 1983-10-22, 1983-10-25, 198"| __truncated__ "1937-11-09, 1937-11-11, 1937-11-13, 1937-11-14, 1937-11-18, 1937-11-23, 1937-11-27, 1937-12-02, 1937-12-04, 193"| __truncated__ ...
    ##  $ data.mostGoalsOneGame           : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ data.mostGoalsOneSeason         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ data.mostGoalsSeasonIds         : chr  "19521953" "19311932, 19321933" "19831984" "19371938" ...
    ##  $ data.mostPenaltyMinutesOneSeason: int  0 0 12 0 2 4 0 0 0 0 ...
    ##  $ data.mostPenaltyMinutesSeasonIds: chr  "19521953" "19311932, 19321933" "19831984" "19371938" ...
    ##  $ data.mostPointsGameDates        : chr  "1952-10-09, 1952-10-11, 1952-10-12, 1952-10-16, 1952-10-18, 1952-10-19, 1952-10-23, 1952-10-25, 1952-10-29, 195"| __truncated__ "1932-03-08, 1932-03-12" "1983-10-06, 1983-10-08, 1983-10-10, 1983-10-13, 1983-10-15, 1983-10-19, 1983-10-20, 1983-10-22, 1983-10-25, 198"| __truncated__ "1937-11-09, 1937-11-11, 1937-11-13, 1937-11-14, 1937-11-18, 1937-11-23, 1937-11-27, 1937-12-02, 1937-12-04, 193"| __truncated__ ...
    ##  $ data.mostPointsOneGame          : int  0 1 0 0 0 0 0 0 0 0 ...
    ##  $ data.mostPointsOneSeason        : int  0 2 0 0 0 0 0 0 0 0 ...
    ##  $ data.mostPointsSeasonIds        : chr  "19521953" "19311932" "19831984" "19371938" ...
    ##  $ data.penaltyMinutes             : int  0 0 12 0 2 4 0 0 0 0 ...
    ##  $ data.playerId                   : int  8444854 8444870 8444917 8444982 8444999 8445028 8445057 8445072 8445091 8445140 ...
    ##  $ data.points                     : int  0 2 0 0 0 0 0 0 0 0 ...
    ##  $ data.positionCode               : chr  "C" "L" "D" "C" ...
    ##  $ data.rookieGamesPlayed          : int  3 10 3 NA 5 2 1 2 1 6 ...
    ##  $ data.rookiePoints               : int  0 2 0 NA 0 0 0 0 0 0 ...
    ##  $ data.seasons                    : int  1 2 1 1 2 1 1 1 1 1 ...
    ##  $ total                           : int  800 800 800 800 800 800 800 800 800 800 ...

``` r
str(rcddt("franchise","detail"))
```

    ## No encoding supplied: defaulting to UTF-8.

    ## 'data.frame':    39 obs. of  14 variables:
    ##  $ data.id                   : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ data.active               : logi  TRUE FALSE FALSE FALSE TRUE TRUE ...
    ##  $ data.captainHistory       : chr  "<ul class=\"striped-list\">\r\n\t<li>Shea Weber: 2018-19 &ndash; Present</li>\r\n\t<li>Max Pacioretty: 2015-16 "| __truncated__ NA NA NA ...
    ##  $ data.coachingHistory      : chr  "<ul class=\"striped-list\">\r\n\t<li>Dominique Ducharme: Feb. 25, 2021&nbsp;&ndash; Present</li>\r\n\t<li>Claud"| __truncated__ NA NA NA ...
    ##  $ data.dateAwarded          : chr  "1917-11-26T00:00:00" "1917-11-26T00:00:00" "1917-11-26T00:00:00" "1917-11-26T00:00:00" ...
    ##  $ data.directoryUrl         : chr  "https://www.nhl.com/canadiens/team/administration" NA NA NA ...
    ##  $ data.firstSeasonId        : int  19171918 19171918 19171918 19191920 19171918 19241925 19241925 19251926 19251926 19261927 ...
    ##  $ data.generalManagerHistory: chr  "<ul class=\"striped-list\">\r\n\t<li>Marc Bergevin: May 2, 2012 &ndash; Present</li>\r\n\t<li>Pierre Gauthier: "| __truncated__ NA NA NA ...
    ##  $ data.heroImageUrl         : chr  "https://records.nhl.com/site/asset/public/ext/hero/Team Pages/MTL/Price.jpg" "https://records.nhl.com/site/asset/public/images/hero/teams/defunct-franchises/montreal-wanderers.jpg" "https://records.nhl.com/site/asset/public/ext/hero/Team Pages/1927SEN.JPG" "https://records.nhl.com/site/asset/public/images/hero/teams/defunct-franchises/hamilton-tigers.jpg" ...
    ##  $ data.mostRecentTeamId     : int  8 41 45 37 10 6 43 51 39 3 ...
    ##  $ data.retiredNumbersSummary: chr  "<ul class=\"striped-list\">\r\n\t<li>1 &ndash;&nbsp;Jacques Plante (1952-63)</li>\r\n\t<li>2 &ndash;&nbsp;Doug "| __truncated__ NA NA NA ...
    ##  $ data.teamAbbrev           : chr  "MTL" "MWN" "SLE" "HAM" ...
    ##  $ data.teamFullName         : chr  "Montréal Canadiens" "Montreal Wanderers" "St. Louis Eagles" "Hamilton Tigers" ...
    ##  $ total                     : int  39 39 39 39 39 39 39 39 39 39 ...

``` r
statdt("teams",1)
```

    ##                                                                                                                                                                            copyright
    ## 1 NHL and the NHL Shield are registered trademarks of the National Hockey League. NHL and NHL team marks are the property of the NHL and its teams. © NHL 2021. All Rights Reserved.
    ##   teams.id        teams.name      teams.link teams.abbreviation
    ## 1        1 New Jersey Devils /api/v1/teams/1                NJD
    ##   teams.teamName teams.locationName teams.firstYearOfPlay
    ## 1         Devils         New Jersey                  1982
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 teams.teamStats
    ## 1 56, NA, 19, 28th, 30, 29th, 7, 15th, 45, 29th, 40.2, 29th, 2.589, 26th, 3.375, 28th, 0.8293, 21st, 14.2, 28th, 22, 28th, 43, 30th, 155, 23rd, 71.0, 31st, 28.7857, 24th, 31.0179, 22nd, 0.552, 22nd, 0.111, 31st, 0.737, 19th, 0.733, 28th, 0.211, 31st, 0.417, 31st, 3180, 8th, 1481, 27th, 1699, 30th, 46.6, 27th, 9, NA, 0.891, NA, NA, 6th, NA, 29th, NA, 24th, 1, 1, New Jersey Devils, New Jersey Devils, /api/v1/teams/1, /api/v1/teams/1, statsSingleSeason, R, Regular season, FALSE
    ##   teams.shortName           teams.officialSiteUrl teams.franchiseId
    ## 1      New Jersey http://www.newjerseydevils.com/                23
    ##   teams.active  teams.venue.name    teams.venue.link teams.venue.city
    ## 1         TRUE Prudential Center /api/v1/venues/null           Newark
    ##   teams.venue.timeZone.id teams.venue.timeZone.offset
    ## 1        America/New_York                          -4
    ##   teams.venue.timeZone.tz teams.division.id teams.division.name
    ## 1                     EDT                25     MassMutual East
    ##    teams.division.link teams.conference.id teams.conference.name
    ## 1 /api/v1/divisions/25                   6               Eastern
    ##   teams.conference.link teams.franchise.franchiseId
    ## 1 /api/v1/conferences/6                          23
    ##   teams.franchise.teamName  teams.franchise.link
    ## 1                   Devils /api/v1/franchises/23

``` r
statdt("franchises",2)
```

    ##                                                                                                                                                                            copyright
    ## 1 NHL and the NHL Shield are registered trademarks of the National Hockey League. NHL and NHL team marks are the property of the NHL and its teams. © NHL 2021. All Rights Reserved.
    ##   franchises.franchiseId franchises.firstSeasonId franchises.lastSeasonId
    ## 1                      2                 19171918                19171918
    ##   franchises.mostRecentTeamId franchises.teamName franchises.locationName
    ## 1                          41           Wanderers                Montreal
    ##        franchises.link
    ## 1 /api/v1/franchises/2

``` r
library(ggplot2)
```
