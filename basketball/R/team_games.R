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
  return(games)
}