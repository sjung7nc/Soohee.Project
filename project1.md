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
```

    ## -- Attaching packages ---------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.0     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter()  masks stats::filter()
    ## x purrr::flatten() masks jsonlite::flatten()
    ## x dplyr::lag()     masks stats::lag()

``` r
rcdURL <- function(list,recd,type,id){
  if (missing(recd) & missing(type) & missing(id)){
    rcdurl <- paste0("https://records.nhl.com/site/api","/",list)
    }  
  else if (missing(type) & missing(id)){
    rcdurl <- paste0("https://records.nhl.com/site/api","/", list,"-", recd)
    }
  else {
    rcdurl <- paste0("https://records.nhl.com/site/api","/", list,"-", recd,"?cayenneExp=", type, "=", id)
    }
  return(rcdurl)
}

rcddt <- function(list,...){
  rcdNHL <- GET(rcdURL(list,...))
  rcdtext <- content(rcdNHL, "text")
  rcdlist <- fromJSON(rcdtext, flatten=TRUE)
  rcdlist <- as.data.frame(rcdlist)
  head(return(rcdlist))
}

statURL <- function(list,id){
  if (missing(id)){
    staturl <- paste0("https://statsapi.web.nhl.com/api/v1/",list,"?expand=team.stats")
  }
  else{
    staturl <- paste0("https://statsapi.web.nhl.com/api/v1/",list,"/",id,"?expand=team.stats")
  }
  return(staturl)
}

statdt <- function(list,...){
  statNHL <- GET(statURL(list,...))
  stattext <- content(statNHL, "text")
  statlist <- fromJSON(stattext, flatten=TRUE)
  statlist <- as.data.frame(statlist)
  head(return(statlist))
}

mostRecentTeamId <- rcddt("franchise")$data.mostRecentTeamId
```

    ## No encoding supplied: defaulting to UTF-8.

``` r
franchiseId <- rcddt("franchise")$data.id
```

    ## No encoding supplied: defaulting to UTF-8.
