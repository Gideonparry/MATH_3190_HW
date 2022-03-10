library(basketball)
library(shiny)
library(reshape2)
library(tidyverse)


download.file("http://kenpom.com/cbbga22.txt","cbb.txt")

cbb = read.fwf("cbb.txt", widths = c(10,24,4,22,5,3,20))

cbb = tibble(cbb)



cbb = kenpom_reformat(cbb)

teams = c()
for (i in 1:length(cbb$Away_team)){
  if (!(cbb$Away_team[i] %in% teams)){
    teams = append(teams, cbb$Away_team[i])
  }
}

ui <- fluidPage(
  selectInput('xcol', 'Team', teams), 
  plotOutput("graph"),
  plotOutput("winbar"),
  tableOutput('winpct'),
  tableOutput('games'),
  plotOutput("diffHist"),
  plotOutput("pointBar")
)

server <- function(input, output){
  output$graph <- renderPlot({
    points_graph(input$xcol,cbb)
  })
  output$winbar <- renderPlot({
    win_graph(input$xcol, cbb)
  })
  output$winpct <- renderTable({win_pct(input$xcol,cbb)})
  output$games <- renderTable({team_games(input$xcol, cbb)})
  output$diffHist <- renderPlot({points_diff_hist(input$xcol, cbb)})
  output$pointBar <- renderPlot({points_bar(input$xcol,cbb)})
}

shinyApp(server = server, ui = ui)
