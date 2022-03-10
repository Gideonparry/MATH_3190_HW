#' Reformats the text file of college basketball games played from kenpom
#' 
#' Takes the text file of games played, and returns a more pretty and complete 
#' version of it. It counts points difference in the games, and was created
#' for use of a shiny app.
#'
#' @param cbb A tibble of college basketball games played
#' 
#'
#' @return A more useful version on the games played for a shiny app 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

kenpom_reformat <- function(cbb){
  
cbb = tibble(cbb)




## Renaming columns

cbb <- rename(cbb, Date = V1)
cbb <- rename(cbb, Away_team = V2)
cbb <- rename(cbb, Away_score = V3)
cbb <- rename(cbb,Home_team = V4)
cbb <- rename(cbb, Home_score = V5)


## Adding score difference

cbb <- cbb %>%
  mutate(score_diff = Home_score - Away_score)



cbb = cbb %>%
  mutate(Date = trimws(Date, which = "both"))

## Formatting date
library(lubridate)
cbb <- cbb %>%
  mutate(Date = mdy(Date))


## Trimming home and away teams


cbb <- cbb %>%
  mutate(Away_team = trimws(Away_team, which = "both"))

cbb <- cbb %>%
  mutate(Home_team = trimws(Home_team, which = "both"))


## Arranging by location

cbb <- cbb %>%
  mutate(V7 = trimws(V7, which = "both"))

cbb <- cbb %>%
  mutate(site = ifelse(is.na(V7),Home_team, V7))
cbb <- cbb %>%
  mutate(nuetral = ifelse(is.na(V7),0,1))

cbb <- arrange(cbb, site)


## Removing colums

cbb <- cbb %>% select(!(V6 | V7))
return(cbb)
}