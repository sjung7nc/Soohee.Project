Project I
================
Soohee Jung
6/11/2021

-   [FUNCTIONS](#functions)
    -   [Record-API Functions](#record-api-functions)
    -   [Stat-API function](#stat-api-function)
    -   [Wrapper function to call the functions
        above](#wrapper-function-to-call-the-functions-above)
-   [FACTORS WHICH INFLUENCE TEAM
    WINNING](#factors-which-influence-team-winning)
    -   [Choose two franchises to compare
        with](#choose-two-franchises-to-compare-with)
    -   [Goals by skater position](#goals-by-skater-position)
    -   [Assists by skater position](#assists-by-skater-position)
    -   [How skater assists affect
        winning?](#how-skater-assists-affect-winning)
    -   [How center position skater’s penalty time affect
        goal?](#how-center-position-skaters-penalty-time-affect-goal)
    -   [IS playing at Home really an
        advantage?](#is-playing-at-home-really-an-advantage)
    -   [Stat](#stat)

# FUNCTIONS

## Record-API Functions

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

# FACTORS WHICH INFLUENCE TEAM WINNING

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
library(ggplot2)
skr <- wrapfnc("record","franchise","skater-records")
p <- ggplot(skr,aes(x=data.positionCode,y=data.goals,fill=data.positionCode))
p+geom_col()+scale_fill_brewer(palette = "Set2")+labs(x="Position",y="Goals",color="Position",title="Goals by Position")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Center position skater scored most goals and then right winger did. That
makes sense!

## Assists by skater position

``` r
a <- ggplot(skr,aes(x=data.positionCode, y=data.assists, fill=data.positionCode))
a+geom_col()+ scale_fill_brewer(palette = "PRGn")+labs(x="Position",y="Goals",color="Position",title="Assists by Position")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Center players assisted most and then Defenders did. I guessed wingers
assisted most but the data tells different story. Interesting!!

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

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The skaters who are in a center position scored more goals as they had
more penalty minutes. What?? Interesting!

``` r
ttotal <- wrapfnc("record","franchise","team-totals")
w <- ggplot(ttotal,aes(data.penaltyMinutes,data.wins))
g <- ggplot(ttotal,aes(data.penaltyMinutes,data.goalsFor))
g+geom_quantile()+labs(x="Penalty Minutes", y="Goals", title="< Penalty minutes and Goals >")
```

    ## Smoothing formula not specified. Using: y ~ x

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
w+geom_quantile()+labs(x="Penalty Minutes", y="Wins", title="< Penalty minutes and Wins >")
```

    ## Smoothing formula not specified. Using: y ~ x

![](README_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

What a surprising result!! I thought the penalty minutes would affects
goals and winnings in negative ways, but the graphs tell us totally
opposite story.

## IS playing at Home really an advantage?

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

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

All teams had over 50% of winning chance when they played at home.
Playing at Home really an advantage!!

## Stat

``` r
stat <- wrapfnc("stat","teams")
# PIck odd number rows
stat <- stat[seq(1,62,2),]
```
