# Libraries
library(shiny)
library(tidyverse)
library(tidymodels)
library(cfbplotR)
library(snakecase)
library(bslib)
library(xgboost)
library(httr2)
library(jsonlite)
library(htmlwidgets)
library(cfbfastR)
library(gt)
library(fontawesome)
library(shinyjs)

# Data:
dfl <- read.csv("gamesModifiedModel2023CondensedLogos.csv") %>% 
       filter(classification == "fbs")                      %>%  
       select(school, opponent, week, matches("Avg"), -matches("Lag"))

# Data:
df2 <- read.csv("gamesModifiedModel2023CondensedLogos.csv") %>% 
  filter(classification == "fbs") %>% 
  select(school, opponent, week, conference, color, matches("yards"), -matches("Avg"), -matches("Lag"))

conferences <- unique(df2$conference)
maxWeek <- max(df2$week)

# Rename some variables
newNames <- snakecase::to_any_case(colnames(dfl), case = "snake")
newNames[which(newNames == "cbs_rank_avg")] <- "cbs_rank"
newNames[which(newNames == "cbs_rank_opp_avg")] <- "cbs_rank_opp"
names(newNames) <- newNames

# Remove Certain things that dont make sense for plotting
badVars <- c("school", "opponent", "week", "season_avg")
newNamesLimited <- sort(newNames[-which(newNames %in% badVars)])

# Rename some variables
newNames2 <- snakecase::to_any_case(colnames(df2), case = "snake")
names(newNames2) <- newNames2

# Remove Certain things that dont make sense for plotting
badVars <- c("school", "opponent", "week", "season_avg", "color", "conference")
newNamesLimited2 <- sort(newNames2[-which(newNames2 %in% badVars)])

# Team Names for user input
teamNames <- sort(unique(dfl$school))

# Requirements
source("helpers/app helper.R")
source("helpers/req.R")
source("helpers/schedule.R")

# FRONTEND ===================================================================================================
ui <- page_sidebar(
  
  # Theme Setter
  theme = bs_theme(
    bg = "#ffeaba", fg = "#252625", primary = "#050505",
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")
  ),
  
  
  # CSS style blueprint:
  tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      h1 {
        font-family: 'Space Mono', sans-serif;
        color:#767676  ;
        font-size: 50px;
      }
      h2 {
        font-family: 'Space Mono', sans-serif;
        color:#1d24a8  ;
        font-size: 50px;
      }
      h4 {
        color: #1d24a8;
        font-weight: bold;
      }
      h5 {
        color: #1d24a8;
      }
      h6 {
        color: #1d24a8;
      }
      body {
        background-color: #f5c962; # Controls color behind card
        color: #191919;
      }
      .shiny-input-container {
        color: #474747; # Controls Box selection color
      }
      p {
        color: #050505;
        border-color: #f5c962;
        border-bottom-style: solid;
        font-size: 20px;
      }
      code {
         font-size: 20px;
         color: #050505;
      }
      .title-container {
        background-color: #252625;  
      }
      .bslib-sidebar-layout > .collapse-toggle {
        padding: 50px 0;
        background-color: #ffeaba;
      }
      
      .bslib-sidebar-layout > .collapse-toggle > .collapse-icon {
        fill: #252625;
      }        
      "
      )
    ),
  # Title
  title = div(
    img(src="icon.png", height = "150x", width = "150px"), 
    img(src="logo.png", height = "100px"),
    class = "title-container"),
  
  
  # Subtitle
  h4('2023 College Football Matchup Simulator'),
  h5('Please click the "Predict" button to get started!'),
  h6('Exhausted Georgia Southern Fan Update'),
  
  
  # Side bar with inputs:
  sidebar = sidebar(
      selectizeInput("team1", "Home Team",   choices = teamNames, multiple = FALSE, selected = teamNames[35]),
      selectizeInput("team2", "Away Team", choices = teamNames, multiple = FALSE, selected = teamNames[3]),
      numericInput("spread", "Home Team's Spread", value = -5, min = -80, max = 80),
      numericInput("moneyLine1", "Home Team's Moneyline",value = -195, min = -12000, max = 12000),
      numericInput("moneyLine2", "Away Team's Moneyline",value = 165, min = -12000, max = 12000),
      numericInput("overUnder",  "Over/Under",value = 45, min = 0, max = 180),
      h6("\n"),
      h6("Enter the above information to use the Matchup Predictor and Quick Compare")
      ),
  
  navset_tab(
    nav_panel(
      title = "Matchup Predictor",
      card(
           card_header("Prediction"), 
           tableOutput("predictionsTable"),
           h4("\n"),
           actionButton("submitBtn", "Predict")
           ),
      card(card_header("Chatbot"), 
           textAreaInput("prompt", label = "Got questions about the prediction result? Ask here:", width = "500px"),
           actionButton("chat", NULL, icon = icon("paper-plane"), width = "100px", class = "m-2 btn-secondary"),
           code("Response:"),
           p(uiOutput("response"))
      ),
      card(fluidRow(gt_output("schedule")))
    ),
    
    nav_panel(
      title = "Quick Compare", 
      card(card_header("Game Averages"), 
           selectizeInput("plotVar", 
                          "Select Variable", 
                          choices = newNamesLimited, 
                          multiple = FALSE,
                          selected = newNamesLimited["total_yards_avg"]
                          ),
           h4(paste("Data through week", maxWeek)),
           plotOutput("comparePlot")
           )
    ),
    
    nav_panel(
      title = "Team Trends",
      
      card(
           card_header("Pick a Conference and a stat"),
           div(
             fluidRow(
              selectizeInput("conference", "Conference", choices = conferences, multiple = FALSE),
              selectizeInput("lineVar", "Line Chart Variable", choices = newNamesLimited2, multiple = FALSE, selected = newNamesLimited2["total_yards"])
            )
           ),
           h4(paste("Data through week", maxWeek)),
           plotOutput("linePlot"),
           actionButton("plotLine", "Plot")
           )
      ),
  ),
    
    # Body
    #card(),
  #
#
    ## Textbox for GPT interaction
    #card(
    #  textAreaInput("prompt", label = "Chat with an AI Georgia Southern Fan!", width = "500px"),
    #  actionButton("chat", NULL, icon = icon("paper-plane"), width = "100px", class = "m-2 btn-secondary"),
    #  p(uiOutput("response"))
    #),
  #
    #code("Game Info:"),
    #fluidRow(gt_output("schedule"))
)

#BACKEND =====================================================================================================
server <- function(input, output, session) {
  # Initialize this for later:
  pTable <- reactiveVal(NULL)

  # Return a table with the predictions of the selected teams.
  predictions <- eventReactive(input$submitBtn, {
    team1 <-  input$team1
    team2 <-  input$team2
    spread <- input$spread
    moneyLine1 <- input$moneyLine1
    moneyLine2 <- input$moneyLine2
    overUnder  <- input$overUnder
    
    # List for if statement below:
    argList <- list(team1, team2, spread, moneyLine1, moneyLine2, overUnder)
 
    # Return predictMatchup dataframe and pull relevant details:
    if (!(list(NULL) %in% argList) & !(list(NA) %in% argList) & !(list("") %in% argList)) {
      pTable_data <- 
        predictMatchup(team1, 
                     team2, 
                     year = 2023, 
                     manualBooks = TRUE, 
                     spreadTeam1 = spread, 
                     moneylineTeam1 = moneyLine1, 
                     moneylineTeam2 = moneyLine2, 
                     overunder = overUnder) %>% 
        select(school, pointsDiffPredFinal, winPred, coverPred, spread) %>%
        mutate(coverPred = if_else(-pointsDiffPredFinal == spread, 
                                   "Push",
                                   if_else(coverPred == 1, "Covers",
                                           "Does Not Cover")),
               winPred   = if_else(abs(pointsDiffPredFinal) == 0, "It's 50/50 :)",
                                 if_else(winPred == 1, "Win" , "Loss"))) %>% 
        rename(School = school,
               `Predicted Score Difference` = pointsDiffPredFinal,
               `Win Prediction` = winPred,
               `Cover Prediction` =  coverPred,
               Spread = spread)
      
      # Extra data for chatbot to look at:
      extraInfo <-
        dfl %>% 
          filter((school == team1 | school == team2) & week == max(week)) %>%
          select(school, totalYardsAvg, totalYardsAllowedAvg, turnoversAllowedAvg, yardsPerRushAttemptAvg,
                 yardsPerRushAttemptAllowedAvg, yardsPerPassAvg, yardsPerPassAllowedAvg, tacklesForLossAvg, week) %>% 
          mutate(across(where(is.numeric), \(X) round(X, digits = 1))) %>% 
          janitor::clean_names(., "sentence")
      
      # This is what the chatBot will see
      feedToChatBot <- left_join(pTable_data,extraInfo, by = "School")
      
      # Initialize reactive variable.
      pTable(paste('Table:', toJSON(feedToChatBot), 'User Input:'))
      pTable_data
    }else{
      raiseMe <- data.frame("Important" = "Please ensure you completely fill out the side boxes!")
      pTable(paste('Table:', toJSON(raiseMe), 'User Input:'))
      raiseMe
     }
    }
  )
  
  # Make some CFB comparison plots.
  cfbplot <- reactive({
    team1 <-  input$team1
    team2 <-  input$team2
    variable <- input$plotVar
    
    # The plot:
    dfl %>% dplyr::filter(school == team1 | school == team2) %>% 
            group_by(school) %>% 
            filter(week == max(week)) %>% 
            ungroup() %>% 
            set_names(newNames) %>% 
            ggplot(aes_string(y= "school", x = variable)) +
            geom_col(aes(color = school, fill = school), linewidth = 2) +
            scale_color_cfb(alt_colors = teamNames) +
            scale_fill_cfb(alpha = .8) +
            theme_minimal() +
            theme(legend.position = "none",
                  panel.grid.major.y = element_blank()) +
            theme(axis.text.y = element_cfb_logo(size = 3)) +
            theme(axis.text.x = element_text(size = 20)) +
            theme(axis.title=element_text(size=20)) +
            ylab("") +
            xlab(to_any_case(variable, "title")) 
   }
  )
  
  # Conference Line Plot:
  cfbPlotLine <- eventReactive(input$plotLine, {
    conference <- input$conference
    lineVar    <- input$lineVar
    
    df2 %>% 
      filter(conference == !!conference) %>%
      set_names(newNames2) %>%
      ggplot(aes_string(x = "week", y = lineVar)) +
      geom_point(aes(color = I(color))) +
      geom_line() +
      geom_area(aes(fill = I(color), color = I(color)), alpha = .5)+
      facet_wrap(~school)+ 
      theme_minimal() +
      theme(strip.text.x = element_text(size = 15),
            axis.title.x = element_text(size = 20),
            axis.text.y = element_text(size = 15),
            axis.text.x = element_text(size = 15)) +
      ylab(to_any_case(lineVar, "title")) +
      xlab("Week")
  })
  
  # Grab output of the prediction dataframe:
  output$predictionsTable <- 
    renderTable(
      expr = {predictions()},
      striped  = T,
      bordered = T)
  
  #Grab output of the plot.
  output$comparePlot <- 
    renderPlot(
      expr = {cfbplot()},  bg="transparent")
  
  # Grab output of line plot:
  output$linePlot <- 
    renderPlot(
      expr = {cfbPlotLine()},  bg="transparent")

  # Obtain GPT Response
  rv <- 
    eventReactive(input$chat, {
      chat_history <- NULL
      message <-  paste(pTable(), input$prompt)
      print(message)
      if(is.null(message) | message == ""){
        response <- "Please type something!"
      }else{
        response <- chat(message, history = chat_history, system_prompt = "general")
      }
   }
  ) 
  
  output$response <- renderUI({rv()}) 
  
  # Schedule table with betting info:
  output$schedule <- render_gt(expr = schedule, width = pct(100), align = "left")
}

# Run the application 
shinyApp(ui = ui, server = server)
