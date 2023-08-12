#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Libraries
library(shiny)
library(tidyverse)
library(tidymodels)
library(cfbplotR)
library(snakecase)
library(bslib)

# Directory:
runnerPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(runnerPath)


# Data:
dfl <- read.csv("gamesModifiedModel2022CondensedLogos.csv") %>% filter(classification == "fbs")
dfl <- dfl %>% select(school, opponent, week, matches("AvgLag"))
newNames <- snakecase::to_any_case(colnames(dfl), case = "snake") 
newNames <- gsub("_lag", "", newNames)
newNamesLimited <- sort(newNames[-c(1,2,3)])
teamNames <- sort(dfl$school)
source("predict matchups.R")

# Create the Shiny UI and Server components
ui <- fluidPage(
  theme = bs_theme(
    bg = "#6BF3A7", fg = "#050505", primary = "#9E0092",
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")
  ),
  
  tags$head(
    tags$style(HTML("
                      #vis1 {
                        border: 2px solid black;
                      }
                      "))
  ),
  
  titlePanel("College Football Prediction Model"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("team1", "School",   choices = teamNames, multiple = FALSE),
      selectizeInput("team2", "Opponent", choices = teamNames, multiple = FALSE),
      numericInput("spread", "Team 1's Spread", value = -7, min = -80, max = 80),
      numericInput("moneyLine1", "School's Moneyline",value = -500, min = -12000, max = 12000),
      numericInput("moneyLine2", "Opponent's Moneyline",value = 350, min = -12000, max = 12000),
      numericInput("overUnder",  "Over/Under",value = 90, min = 0, max = 180),
      selectizeInput("plotVar", "X-Axis Variable", choices = newNamesLimited, multiple = FALSE,
                     selected = newNamesLimited[74]),
      actionButton("submitBtn", "Get Predictions")
    ),
    mainPanel(
      tableOutput("predictionsTable"),
      plotOutput("comparePlot")
    ),
   
  )
)

server <- function(input, output) {
  
  # Return a table with the predictions of the selected teams.
  predictions <- eventReactive(input$submitBtn, {
    team1 <-  input$team1
    team2 <-  input$team2
    spread <- input$spread
    moneyLine1 <- input$moneyLine1
    moneyLine2 <- input$moneyLine2
    overUnder  <- input$overUnder
    
    if (!is.null(team1) && !is.null(team2)) {
      predictMatchup(team1, 
                     team2, 
                     year = 2022, 
                     manualBooks = T, 
                     spreadTeam1 = spread, 
                     moneylineTeam1 = moneyLine1, 
                     moneylineTeam2 = moneyLine2, 
                     overunder = overUnder) %>% 
        select(school, pointsDiffPredFinal, winPred, coverPred, spread) %>%
        mutate(coverPred = if_else(coverPred == 1, "Covers",
                                   if_else(abs(pointsDiffPredFinal) == abs(spread), 
                                           "House Wins", 
                                           "Does Not Cover")),
               winPred   = if_else(abs(pointsDiffPredFinal) == 0, "It's 50/50 :)",
                                 if_else(winPred == 1, "Win" , "Loss"))) %>% 
        rename(School = school,
               `Predicted Score Difference` = pointsDiffPredFinal,
               `Win Prediction` = winPred,
               `Cover Prediction` =  coverPred,
               Spread = spread)
    }
  }
  

  
  )
  
  # Make some CFB comparison plots.
  cfbplot <- eventReactive(input$submitBtn, {
    team1 <-  input$team1
    team2 <-  input$team2
    variable <- input$plotVar
    
    dfl %>% dplyr::filter(school == team1 | school == team2) %>% 
            group_by(school) %>% 
            filter(week == max(week)) %>% 
            ungroup() %>% 
            set_names(newNames) %>% 
            ggplot(aes_string(y= "school", x = variable)) +
            geom_col(aes(color = school, fill = school), size = 2) +
            scale_color_cfb(alt_colors = teamNames) +
            scale_fill_cfb(alpha = .8) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid.major.y = element_blank()) +
            theme(axis.text.y = element_cfb_logo(size = 3)) +
            theme(axis.text.x = element_text(size = 30)) +
            theme(axis.title=element_text(size=30)) +

            ylab("") +
            xlab(to_any_case(variable, "title"))
    
    
  })
  
  output$predictionsTable <- renderTable(
    expr = {predictions()},
    striped  = T,
    bordered = T)
  
  output$comparePlot <- renderPlot(
    expr = {cfbplot()},  bg="transparent"
  )
}
# Run the application 
shinyApp(ui = ui, server = server)
