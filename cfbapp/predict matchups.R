######################################
# Purpose:
# Create function that predicts
# win/cover/score difference between
# any two teams, by week.
######################################

# Libraries
library(tidyverse)
library(tidymodels)

df  <- read.csv("gamesModifiedModel2022Condensed.csv") 

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
