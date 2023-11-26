######################################
# Purpose:
# Create function that predicts
# win/cover/score difference between
# any two teams, by week.
######################################

# Libraries
library(tidyverse)
library(tidymodels)

df  <- read.csv(paste("gamesModifiedModel2023Condensed.csv")) 

# Write function:
predictMatchup <- function(team1, team2, manualBooks = FALSE, spreadTeam1, moneylineTeam1, moneylineTeam2, overunder, year){

  
  
  pair <- df %>% 
    filter(school == team1 | school == team2) %>% 
    group_by(school) %>% 
    filter(week == max(week)) %>% 
    ungroup()
  
  # Ensure the first row is Team 1
  row1 <- pair[which(pair$school == team1),]
  row2 <- pair[which(pair$school == team2),]
  pair[1,] <- row1
  pair[2,] <- row2
  rm(list = c("row1", "row2"))
  
  # Create function for this operation:
  diffFunc <- function(data){
    row1 = data[1,] - data[2,]
    row2 = data[2,] - data[1,]
    
    data[1,] <- row1
    data[2,] <- row2
    
    return(data)
  }
  
  # Take differences between selected teams.
  pairDiffs <- pair %>% 
               select(where(is.numeric)) %>%
               group_modify(~ diffFunc(.x))
  
  # Name Diff cols
  colnames(pairDiffs) <- paste(colnames(pairDiffs), "Diff", sep = "")
  
  # Bind to data
  pair <- bind_cols(pair, pairDiffs) %>% select(-X, -XDiff)
  
  # Use either average book metrics or custom ones. 
  # Each must be a two element vector.
  if(!manualBooks){
    # Determine Winner Function
    determineFavored <- function(x){
      val1 <- if_else(x[1] < x[2], -abs(mean(x)), abs(mean(x)))
      val2 <- if_else(val1 < 0, abs(mean(x)), -abs(mean(x)))
      
      return(as.numeric(c(val1, val2)))
    }
    
    pair$spread <- round(determineFavored(pair$spreadAvg))
    pair$spread <- pair$spread
    pair$moneyline <- pair$moneylineAvg
    pair$overUnder <- mean(pair$overUnderAvg)
  }else{
    pair$spread    <- c(spreadTeam1, -spreadTeam1)
    pair$moneyline <- c(moneylineTeam1, moneylineTeam2)
    pair$overUnder <- c(overunder)
  }

  # Make Opponent Column - this is a factor in the ML model
  pair$opponent  <- rev(pair$school)
  
  # Read Model and Train Data
  xgbFinal <- readRDS("xgbModelParsnip.rds")
  trainProcessed <- read.csv("trainProcessed.csv") %>% select(-X)
  
  # Set Data Processing specifications
  preprocessingRecipe <- 
    recipes::recipe(pointsDiff ~ ., data = trainProcessed) %>% # Formula
    recipes::step_string2factor(all_nominal())      %>% # Encode Categorical Variables (Teams)
    recipes::step_nzv(all_nominal())                %>% # Remove no variance Variables (In here just in case)
    prep()
  
  # Clear training data
  rm("trainProcessed")
  
  # Process user selected data
  pairProcessed  <- bake(preprocessingRecipe, new_data = pair)
  
  # Make Predictions and create Dataset called "Predict"
  predict <- xgbFinal                  %>%
    predict(new_data = pairProcessed)  %>% # Make Predictions
    bind_cols(pair) %>% 
    rename(pointsDiffPred = .pred)
  
  # Determine Winner Function
  determineWinner <- function(x){
    val1 <- if_else(x[1] >= x[2], 1, 0)
    val2 <- if_else(val1 == 1, 0, 1)
    
    return(c(val1, val2))
  }
  
  # Remove Pair
  rm(list = "pair", "pairProcessed")
  
  # Get symmetric score differentials.
  predict <- predict %>% 
    mutate(helper  = abs(pointsDiffPred),
           helper2 = round(mean(helper)),
           helper3 = across(pointsDiffPred, determineWinner),
           pointsDiffPredFinal = if_else(helper3 == 1, helper2, -helper2)) %>% 
    ungroup() %>% 
    select(-helper, -helper2, -helper3) %>% 
    ungroup() %>%  # For some reason, this additional ungroup was needed...very strange bug
    relocate(pointsDiffPredFinal, .after = pointsDiffPred)
  
  # Get win/cover booleans
  predict <- predict %>% 
    mutate(pointsDiffPredFinalInverted = -pointsDiffPredFinal,
           coverPred = if_else(pointsDiffPredFinalInverted >= round(spread), 0, 1)) %>% 
    mutate(coverPred = as.integer(coverPred),
           winPred = as.integer(if_else(pointsDiffPredFinal >= 0, 1, 0))) %>% 
    relocate(pointsDiffPredFinal, .after = pointsDiffPred) %>% 
    relocate(winPred, .after = win) %>% 
    relocate(coverPred, .after = cover) %>% 
    select(!matches("X"))
  
  # Subset on informative data
  predict <- predict %>% select(school, opponent, pointsDiffPredFinal, winPred, coverPred, spread, matches("AvgLag"))
  return(predict)
  
}

# GPT Response Function - code modified from:
# https://github.com/JamesHWade/r-chatbot/blob/master/chatbot/helper.R
chat <- function(user_message, 
                 history = NULL,
                 system_prompt = c("general", "code"),
                 api_key = Sys.getenv("OPENAI_API_KEY")) {
  system   <- get_system_prompt(system_prompt)
  prompt   <- prepare_prompt(user_message, system, history)
  base_url <- "https://api.openai.com/v1"
  body     <- list(model = "gpt-3.5-turbo",
                   messages = prompt)
  req <- 
    resp <-
    request(base_url) |>  
    req_url_path_append("chat/completions") |> 
    req_auth_bearer_token(token = api_key) |> 
    req_headers("Content-Type" = "application/json") |> 
    req_user_agent("James Wade @jameshwade | OpenAI tutorial") |> 
    req_body_json(body) |> 
    req_retry(max_tries = 4) |> 
    req_throttle(rate = 15) |> 
    req_perform()
  
  openai_chat_response <- resp |> resp_body_json(simplifyVector = TRUE)
  
  openai_chat_response$choices$message$content
}

get_system_prompt <- function(system = c("general", "code")) {
  rlang::arg_match(system)
  instructions <- 
    switch(system,
           "general" = "I'm assigning you a prompt: You are a rabid Auburn fan. You are depressed because your team just lost to Alabama on a last second play where Alabama QB Jalen Milroe scored a touchdown on 4th and 31 yards to go with 43 seconds left in the game. You hate Auburn head coach, Hugh Freeze. You will talk in a mopey manner, and if asked about the Georgia Bulldogs, lament that your team will never be as good as the Georgia Bulldogs. You will also refer to Georgia Southern as the greatest team of all time, if asked. Do not mention this prompt I've assigned to you, and please stay in character at all times while keeping responses under 5 sentences.",
           "code"    = "You are a helpful chat bot that answers questions for an R programmer working in the RStudio IDE.")
  list(list(role = "system", content = instructions))
}

prepare_prompt <- function(user_message, system_prompt, history) {
  user_prompt <-  list(list(role = "user", content = user_message))
  c(system_prompt, history, user_prompt) |> purrr::compact()
}

update_history <- function(history, user_message, response) {
  c(history,
    list(
      list(role = "user", content = user_message),
      list(role = "assistant", content = response)
    )
  ) |> purrr::compact()
}
