# Purpose:
# Test Prediction Perfomance and modify predictions to be symmetric per game

# Libraries
library(tidyverse)
library(tidymodels)

# Data
df <- read.csv(paste("downstream/gamesModifiedModel", year, ".csv", sep = ""))

# Prediction Adjustment - Test Here (not implemented)
df$pointsDiffPredAdj <- df$pointsDiffPred - .5*df$spreadAvg

# RMSE
checks <- paste("Overall RMSE of Sportbook Spread (Untrained Data):", 
                round(rmse(df %>% filter(week > max(week) -1), pointsDiff, spreadInverted)[3], digits = 2),
               "Overall RMSE of Unadjusted Prediction (Untrained Data):", 
                round(rmse(df %>% filter(week > max(week) -1), pointsDiff, pointsDiffPred)[3], digits = 2),
               "Overall RMSE of Adjusted Prediction (Untrained Data):", 
                round(rmse(df %>% filter(week > max(week) -1), pointsDiff, pointsDiffPredAdj)[3], digits = 2),
               sep = "\n"
               )

cat(checks, "\n")
rm("checks")

# Determine which predicted score means win for a team
determineWinner <- function(x){
  val1 <- if_else(x[1] >= x[2], 1, 0)
  val2 <- if_else(val1 == 1, 0, 1)
  
  return(c(val1, val2))
}

# Make symmetric version of  Prediction, average, and round it.
df2 <- 
  df %>% 
    group_by(gameId) %>%
    mutate(helper  = abs(pointsDiffPred),
           helper2 = round(mean(helper)),
           helper3 = across(pointsDiffPred, determineWinner),
           pointsDiffPredFinal = if_else(helper3 == 1, helper2, -helper2)) %>% 
    ungroup() %>% 
    select(-helper, -helper2, -helper3) %>% 
    ungroup()
     
# Wow... beats the spread prediction by 2 points on average
paste("Final Symmetric Prediction RMSE (Untrained Data, Week > current):", round(rmse(df2 %>% filter(week > max(week)-1), pointsDiff, pointsDiffPredFinal)[3], digits = 2))

# Quick Spread coverage evaluator
df3 <- 
  df2 %>% 
    mutate(pointsDiffPredFinalInverted = -pointsDiffPredFinal,
           coverPred = if_else(pointsDiffPredFinalInverted >= spreadOpen, 0, 1)) %>% 
    drop_na(coverPred) %>% 
    mutate(coverPred = as.integer(coverPred),
           winPred = as.integer(if_else(pointsDiffPredFinal >= 0, 1, 0))) %>% 
    filter(week > max(week) -1)

# Cover Performance:
print(caret::confusionMatrix(as.factor(df3$coverPred), as.factor(df3$cover), positive = "1"))

# Win Loss Performance:
print(caret::confusionMatrix(as.factor(df3$winPred), as.factor(df3$win), positive = "1"))

df4 <- 
  df2 %>% 
    mutate(pointsDiffPredFinalInverted = -pointsDiffPredFinal,
           coverPred = if_else(pointsDiffPredFinalInverted >= spreadOpen, 0, 1)) %>% 
    mutate(coverPred = as.integer(coverPred),
           winPred = as.integer(if_else(pointsDiffPredFinal >= 0, 1, 0))) %>% 
    relocate(pointsDiffPredFinal, .after = pointsDiffPred) %>% 
    relocate(winPred, .after = win) %>% 
    relocate(coverPred, .after = cover) %>% 
    select(!matches("X"))

# Save Data:
write.csv(df4, paste("downstream/gamesModifiedModel", year, ".csv", sep = "")) 

# Save Data with select variables:
df5 <- df4 %>% select(!matches("Diff")) %>% ungroup()
write.csv(df5, paste("downstream/gamesModifiedModel", year, "Condensed.csv", sep = ""))
write.csv(df5, paste("cfbapp/gamesModifiedModel", year, "Condensed.csv", sep = ""))

# Clear Memory
rm(list = setdiff(c("year", "runnerPath"), ls()))
