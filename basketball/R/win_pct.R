#' Returns winning percentage, wins and losses for a given college basketball team
#' 
#' Input a given team and the data frame of games to see a winning percentage,
#' and number of wins and losses
#'  
#'
#' @param team_name A description of the input parameter \code{team_name}
#' @param df the data frame containing college basketabll games \code{df}
#'
#' @return Returns the winning percentage of the chosen team
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' suu_wpc <- win_pct("Southern Utah", cbb)

win_pct <- function(team_name, df){
  team = df %>% summarize(
    team = team_name,
    win_pct = ((sum(Home_team == team_name & score_diff > 0) + sum(Away_team == team_name & score_diff < 0))
               /(sum(Home_team == team_name | Away_team == team_name))),
    wins = (sum(Home_team == team_name & score_diff > 0) + sum(Away_team == team_name & score_diff < 0)),
    losses = (sum(Home_team == team_name & score_diff < 0) + sum(Away_team == team_name & score_diff > 0))
    
  )
  return (team)
}