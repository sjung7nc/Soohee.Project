Project I
================
Soohee Jung
6/11/2021

-   [FUNCTIONS](#functions)
    -   [Record API Functions](#record-api-functions)
    -   [stat API function](#stat-api-function)
    -   [wrapper function to call the functions
        above](#wrapper-function-to-call-the-functions-above)
-   [FACTORS WHICH INFLUENCE TEAM
    WINNING](#factors-which-influence-team-winning)
    -   [Choose two franchises to compare
        with](#choose-two-franchises-to-compare-with)
    -   [How skater assists and penalty time affect to winning or
        goals?](#how-skater-assists-and-penalty-time-affect-to-winning-or-goals)
    -   [How goalie goal against affect to
        winning?](#how-goalie-goal-against-affect-to-winning)

# FUNCTIONS

## Record API Functions

``` r
library(httr)
library(jsonlite)
library(tidyverse)

# to get the franchise ids and the team names
frurl <- GET("https://records.nhl.com/site/api/franchise")
frtext <- content(frurl, "text", encoding = "UTF-8")
frlist <- fromJSON(frtext, flatten=TRUE)
frlist <- as.data.frame(frlist)
frtbl <- tibble(frlist$data.id, frlist$data.fullName)

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
  rcdtext <- content(rcdNHL, "text",encoding = "UTF-8")
  rcdlist <- fromJSON(rcdtext, flatten=TRUE)
  rcdlist <- as.data.frame(rcdlist)
  return(rcdlist)
}
```

## stat API function

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
  return(statlist)
}
```

## wrapper function to call the functions above

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

# FACTORS WHICH INFLUENCE TEAM WINNING

## Choose two franchises to compare with

I will compare franchise with id number 3 and 5. Let’s find the factors
which affect team winning!

``` r
# I will compare these two Franchises how players affect the winning!
frwins <- wrapfnc("record","franchise", "team-totals") %>%
   filter((data.franchiseId==3 | data.franchiseId==5) & data.gameTypeId==2) %>%
   select("Franchise ID"=data.franchiseId, "Team Name"=data.teamName, "Total Wins"=data.wins) 

AvgWin <- frwins %>% group_by(`Franchise ID`) %>% mutate(AvgWin=mean(`Total Wins`)) %>% distinct(`Franchise ID`, AvgWin)
knitr::kable(AvgWin)
```

| Franchise ID | AvgWin |
|-------------:|-------:|
|            5 | 1000.0 |
|            3 |  134.5 |

## How skater assists and penalty time affect to winning or goals?

``` r
skid3 <- wrapfnc("record","franchise", "skater-records","franchiseId",3)
skid5 <- wrapfnc("record","franchise", "skater-records","franchiseId",5)
skid35 <- rbind(skid3,skid5) %>% select(data.franchiseId,data.playerId,data.assists,data.goals,data.penaltyMinutes)
```

## How goalie goal against affect to winning?

A good goals against average will fall between 2.00 and 2.70 for NHL
goaltenders. Anything between 2.70-3.00 is considered respectable, while
below 2.00 is very exceptional.

``` r
goid3 <- wrapfnc("record","franchise", "goalie-records","franchiseId",3)
goid5 <- wrapfnc("record","franchise", "goalie-records","franchiseId",5)
goid35 <- rbind(goid3,goid5) %>% select(data.franchiseId,data.playerId,data.mostGoalsAgainstOneGame)
```
