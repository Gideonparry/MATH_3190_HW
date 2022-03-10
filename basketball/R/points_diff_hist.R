#' Creates a plot of points, for, against, and difference over time for a team
#' 
#' Takes a team name and tibble of college basketball games played and returns
#' a histogram of their points differnces in their games.
#' 
#' @param team_name The name of a team \code{inputParameter1}
#' @param df data fram containg college basketball games played \code{inputParameter2}
#'
#' @return Graph of points over time 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' points_graph("Gonzaga", cbb)

points_diff_hist <- function(team_name,df){
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
    select(Date | opponent | site | result | points_for | points_against | score_diff)
  
  return(qplot(games$score_diff, geom="histogram", binwidth = 5, fill=I("blue")))
  
}