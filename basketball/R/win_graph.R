#' Creates a plot of points, for, against, and difference over time for a team
#' 
#' Takes a team name and tibble of college basketball games played and returns
#' a bar graph of wins and losses.
#' A green line representing points for, a red line representing poins against, and
#' a blue line representing points difference.Requires ggplot2 package
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

win_graph <- function(team_name, df){
  team = df %>% summarize(
    team = team_name,
    win_pct = ((sum(Home_team == team_name & score_diff > 0) + sum(Away_team == team_name & score_diff < 0))
               /(sum(Home_team == team_name | Away_team == team_name))),
    wins = (sum(Home_team == team_name & score_diff > 0) + sum(Away_team == team_name & score_diff < 0)),
    losses = (sum(Home_team == team_name & score_diff < 0) + sum(Away_team == team_name & score_diff > 0))
    
  )
  
  
  team <- melt(team)
  team <- team[-1,]
  p<-ggplot(data=team, aes(x=variable, y=value, fill = variable)) +
    geom_bar(stat="identity") +
    labs(title = "Wins and losses",
         x = "", y = "") +
    scale_fill_manual(values = c("dark green", "red"))
  
  return (p)
}