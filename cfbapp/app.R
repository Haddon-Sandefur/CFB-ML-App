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
library(DT)

# Data:
data_browser <- 
  read.csv("gamesModifiedModel2023CondensedLogos.csv") %>% 
  select(school, opponent, week, points, pointsAllowed, matches("yards"), matches("Tds"), 
         matches("tackles"), matches("turnover"), matches("down"), -matches("Avg"), -matches("Lag"))

primary_data <- 
  read.csv("gamesModifiedModel2023CondensedLogos.csv") %>% 
       filter(classification == "fbs") %>%  
       select(school, opponent, week, matches("Avg"), -matches("Lag"))

# Data:
trend_data <- 
  read.csv("gamesModifiedModel2023CondensedLogos.csv") %>% 
    filter(classification == "fbs") %>% 
    select(school, opponent, week, conference, color, 
           matches("yards"), matches("turnover"), matches("Tds"),
           matches("turnouver"), -matches("Avg"), -matches("Lag"),
         -matches("totalPenalties"))


conferences <- unique(trend_data$conference)
maxWeek <- max(trend_data$week)

# Rename some variables
primary_names <- snakecase::to_any_case(colnames(primary_data), case = "snake")
primary_names[which(primary_names == "cbs_rank_avg")] <- "cbs_rank"
primary_names[which(primary_names == "cbs_rank_opp_avg")] <- "cbs_rank_opp"
names(primary_names) <- primary_names

# Remove Certain things that dont make sense for plotting
primary_bad_vars <- c("school", "opponent", "week", "season_avg")
primary_names_limit <- sort(primary_names[-which(primary_names %in% primary_bad_vars)])

# Rename some variables
trend_names <- snakecase::to_any_case(colnames(trend_data), case = "snake")
names(trend_names) <- trend_names

# Remove Certain things that dont make sense for plotting
trend_bad_vars <- c("school", "opponent", "week", "season_avg", "color", "conference")
trend_names_limit <- sort(trend_names[-which(trend_names %in% trend_bad_vars)])

# Team Names for user input
teamNames <- sort(unique(primary_data$school))
names(teamNames) <- teamNames

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
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css")
    ),
  
  # Title
  title = div(
    img(src="logo.png", height = "100px"),
    class = "title-container"),
  
  # Subtitle
  h4('2023 College Football Matchup Simulator'),
  h6('New UI Update!'),
  
  # Side bar with inputs:
  sidebar = sidebar(
      selectizeInput("team1", "Home Team",   choices = teamNames, multiple = FALSE, selected = teamNames["Michigan"]),
      selectizeInput("team2", "Away Team", choices = teamNames, multiple = FALSE, selected = teamNames["Alabama"]),
      numericInput("spread", "Home Team's Spread", value = -2.5, min = -80, max = 80),
      numericInput("moneyLine1", "Home Team's Moneyline",value = -125, min = -12000, max = 12000),
      numericInput("moneyLine2", "Away Team's Moneyline",value = 105, min = -12000, max = 12000),
      numericInput("overUnder",  "Over/Under",value = 45, min = 0, max = 180),
      h6("\n"),
      h6("Enter the above information to use the Matchup Predictor and Quick Compare")
      ),
  
  # Pages
  navset_tab(
    
    # Page1
    nav_panel(
      title = "Matchup Predictor",
      
      # Prediction Output
      card(
           card_header("Prediction"), 
           tableOutput("predictionsTable"),
           h4("\n"),
           actionButton("submitBtn", "Predict")
           ),
      card(card_header("Quick Compare"), 
           selectizeInput("plotVar", 
                          "Select Variable", 
                          choices = primary_names_limit, 
                          multiple = FALSE,
                          selected = primary_names_limit["total_yards_avg"]
           ),
           h6(paste("Data through week", maxWeek)),
           plotOutput("comparePlot")
      ),
      # Chatbot
      card(
           card_header("Chatbot"), 
           textAreaInput("prompt", label = "Got questions about the prediction result? Ask here:", width = "500px"),
           actionButton("chat", NULL, icon = icon("paper-plane"), width = "100px", class = "m-2 btn-secondary"),
           code("Response:"),
           p(uiOutput("response"))
          ),
      # Previous/Upcoming Games
      card(card_header("Upcoming Games"),
           fluidRow(gt_output("schedule")))
    ),
    
    
    # Page 3
    nav_panel(
      title = "Team Trends",
      card(
           card_header("Pick a Conference and a stat"),
           div(
             fluidRow(
              selectizeInput("conference", "Conference", choices = conferences, multiple = FALSE),
              selectizeInput("lineVar", "Line Chart Variable", choices = trend_names_limit, multiple = FALSE, selected = trend_names_limit["total_yards"])
            )
           ),
           h4(paste("Data through week", maxWeek)),
           plotOutput("linePlot"),
           )
      ),
    
    # Page 4
    nav_panel(
      title = "Data Explore",
      card(
        min_height = 1000,
        card_header("Pick a Team"),
        selectizeInput("dataTeam", "Team", choices = teamNames, multiple = FALSE),
        dataTableOutput("dataBrowse")
      )
    ),
  ),
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
        primary_data %>% 
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
    primary_data %>% dplyr::filter(school == team1 | school == team2) %>% 
            group_by(school) %>% 
            filter(week == max(week)) %>% 
            ungroup() %>% 
            set_names(primary_names) %>% 
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
  cfbPlotLine <- reactive({
    conference <- input$conference
    lineVar    <- input$lineVar
    
    trend_data %>% 
      filter(conference == !!conference) %>%
      set_names(trend_names) %>%
      ggplot(aes_string(x = "week", y = lineVar)) +
      geom_point(aes(color = I(color))) +
      geom_line() +
      geom_area(aes(fill = I(color), color = I(color)), alpha = .5)+
      facet_wrap(~school)+ 
      theme_classic() +
      theme(strip.text.x = element_text(size = 15),
            axis.title.x = element_text(size = 20),
            axis.text.y = element_text(size = 15),
            axis.text.x = element_text(size = 15)) +
      ylab(to_any_case(lineVar, "title")) +
      xlab("Week")
  })
  
  # Obtain GPT Response
  gptResponse <- 
    eventReactive(input$chat, {
      chat_history <- NULL
      message <-  paste(pTable(), input$prompt)
      print(message)
      if(is.null(input$prompt) | input$prompt == ""){
        response <- "Please type something!"
      }else{
        response <- chat(message, history = chat_history, system_prompt = "general")
      }
    }
    ) 
  
  browse <-
    reactive({
      team <- input$dataTeam
      
      data_browser %>% 
        filter(school == !!team)
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
  
  output$response <- renderUI({gptResponse()}) 
  
  # Schedule table with betting info:
  output$schedule <- render_gt(expr = schedule, width = pct(100), align = "left")
  
  # Output data table
  output$dataBrowse <- renderDataTable(
    expr = {browse()}
  )
}

# Run the application 
shinyApp(ui = ui, server = server)