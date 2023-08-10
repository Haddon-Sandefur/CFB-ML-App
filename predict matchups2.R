######################################
# Purpose:
# Create function that predicts
# win/cover/score difference between
# any two teams, by week.
######################################

# Libraries
library(tidyverse)
library(tidymodels)

# Global:
year =  2022

# Data:
df <- read.csv(paste("downstream/gamesModifiedModel", year, "Condensed.csv", sep = ""))

# Write function:
predictMatchup <- function(data, team1, team2, week, manualBooks = FALSE, spread, moneyline, overunder){
  
}


pair <- df %>% 
        filter(school == "Duke" | school == "Georgia Southern", week == 13) 

# Create function for this operation:
diffFunc <- function(data){
  row1 = data[1,] - data[2,]
  row2 = data[2,] - data[1,]
  
  data[1,] <- row1
  data[2,] <- row2
  
  return(data)
}

pairDiffs <- pair %>% 
             select(where(is.numeric)) %>%
             group_modify(~ diffFunc(.x))
colnames(pairDiffs) <- paste(colnames(pairDiffs), "Diff", sep = "")
pair <- bind_cols(pair, pairDiffs) %>% select(-X, -XDiff)

pair$spread    <- pair$spreadAvg
pair$moneyline <- pair$moneylineAvg
pair$overUnder <- pair$overUnderAvg
pair$opponent  <- rev(pair$school)

xgbFinal <- readRDS("model/xgbModelParsnip.rds")
trainProcessed <- read.csv("downstream/trainProcessed.csv") %>% select(-X)

preprocessingRecipe <- 
  recipes::recipe(pointsDiff ~ ., data = trainProcessed) %>% # Formula
  recipes::step_string2factor(all_nominal())      %>% # Encode Categorical Variables (Teams)
  recipes::step_nzv(all_nominal())                %>% # Remove no variance Variables (In here just in case)
  prep()


pairProcessed  <- bake(preprocessingRecipe, new_data = pair)

predict <- xgbFinal                                             %>%
           predict(new_data = pairProcessed)                    %>% # Make Predictions
           bind_cols(pair) %>% 
           rename(pointsDiffPred = .pred)

determineWinner <- function(x){
  val1 <- if_else(x[1] >= x[2], 1, 0)
  val2 <- if_else(val1 == 1, 0, 1)
  
  return(c(val1, val2))
}

predict <- predict %>% 
           mutate(helper  = abs(pointsDiffPred),
                  helper2 = round(mean(helper)),
                  helper3 = across(pointsDiffPred, determineWinner),
                  pointsDiffPredFinal = if_else(helper3 == 1, helper2, -helper2)) %>% 
           ungroup() %>% 
           select(-helper, -helper2, -helper3) %>% 
           ungroup() %>% 
           relocate(pointsDiffPredFinal, .after = pointsDiffPred)

predict <- predict %>% 
           mutate(pointsDiffPredFinalInverted = -pointsDiffPredFinal,
                  coverPred = if_else(pointsDiffPredFinalInverted >= round(spreadAvgLagDiff), 0, 1)) %>% 
           mutate(coverPred = as.integer(coverPred),
                  winPred = as.integer(if_else(pointsDiffPredFinal >= 0, 1, 0))) %>% 
           relocate(pointsDiffPredFinal, .after = pointsDiffPred) %>% 
           relocate(winPred, .after = win) %>% 
           relocate(coverPred, .after = cover) %>% 
           select(!matches("X"))



