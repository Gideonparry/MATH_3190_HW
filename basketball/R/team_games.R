#' Filters college basketball data to one specific team
#' 
#' Using data of college basketball games played from kenpom.com
#' this function takes the name of a team and a tibble of all games played
#' and returns games played by that team.Change score_diff from home-away to
#' positive if the given team won, and negative if it lost.
#'
#' @param team_name The name of a college basketball team \code{inputParameter1}
#' @param df The data frame that contains all the college basketball games played \code{inputParameter2}
#'
#' @return Returns only the games played by the specified team
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' Southern_Utah games <- team_games("Southern Utah", cbb)

team_games <- function(team_name,df){
  games <- df %>%
    filter(Home_team == team_name | Away_team == team_name)
  games <- games %>%
    mutate(score_diff = ifelse(Away_team == team_name, -score_diff, score_diff))
  games <- games %>%
    mutate(points_for = ifelse(Away_team == team_name, Away_score, Home_score))
  games <- games %>%
    mutate(points_against = ifelse(Away_team == team_name, Home_score, Away_score))
  games <- arrange(games, Date)
  games <- games %>%
    mutate(site = ifelse((Home_team == team_name) & (Home_team == site),"Home", 
                         ifelse((Away_team == team_name) & (Home_team == site),"Away",site)))
  games <- games %>%
    mutate(opponent = ifelse(Home_team == team_name, Away_team, Home_team))
  games <- games %>%
    mutate(result = ifelse(score_diff > 0 ,"win", "loss")) %>%
    mutate(Date = as.character(Date)) %>%
    select(Date | opponent | site | result | points_for | points_against) %>%
    mutate(points_for = format(round(points_for, 2), nsmall = 2) ) %>%
    mutate(points_against = format(round(points_against, 2), nsmall = 2) )
   
    
  
  return(games)
}