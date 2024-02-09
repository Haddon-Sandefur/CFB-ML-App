# Purpose:
# Test Prediction Performance and modify predictions to be symmetric per game

# Data
df <- read.csv(paste("downstream/gamesModifiedModel", year, ".csv", sep = ""))

# Prediction Adjustment - Test Here (not implemented)
df$pointsDiffPredAdj <- df$pointsDiffPred - .5*df$spreadAvg

# RMSE
checks <- paste("Overall RMSE of Sportbook Spread (Untrained Data):", 
                round(rmse(df %>% filter(week %in% testWeeks), pointsDiff, spreadInverted)[3], digits = 2),
               "Overall RMSE of Unadjusted Prediction (Untrained Data):", 
                round(rmse(df %>% filter(week %in% testWeeks), pointsDiff, pointsDiffPred)[3], digits = 2),
               "Overall RMSE of Adjusted Prediction (Untrained Data):", 
                round(rmse(df %>% filter(week %in% testWeeks), pointsDiff, pointsDiffPredAdj)[3], digits = 2),
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
     
paste("Final Symmetric Prediction RMSE (Untrained Data, Week > current):", round(rmse(df2 %>% filter(week %in% testWeeks), pointsDiff, pointsDiffPredFinal)[3], digits = 2))
cat('\n')

# Quick Spread coverage evaluator
df3 <- 
  df2 %>% 
    filter(week %in% testWeeks) %>% 
    mutate(pointsDiffPredFinalInverted = -pointsDiffPredFinal,
           coverPred = if_else(pointsDiffPredFinalInverted >= spread, 0, 1)) %>% 
    drop_na(coverPred) %>% 
    mutate(coverPred = as.integer(coverPred),
           winPred = as.integer(if_else(pointsDiffPredFinal >= 0, 1, 0)))

# Cover Performance:
cat('Confusion Matrix for Cover Predictions:', '\n')
print(caret::confusionMatrix(as.factor(df3$coverPred), as.factor(df3$cover), positive = "1"))

# Win Loss Performance:
cat('Confusion Matrix for Win/Loss Predictions:', '\n')
print(caret::confusionMatrix(as.factor(df3$winPred), as.factor(df3$win), positive = "1"))

df4 <- 
  df2 %>% 
    mutate(pointsDiffPredFinalInverted = -pointsDiffPredFinal,
           coverPred = if_else(pointsDiffPredFinalInverted >= spread, 0, 1)) %>% 
    mutate(coverPred = as.integer(coverPred),
           winPred = as.integer(if_else(pointsDiffPredFinal >= 0, 1, 0)),
           coverCorrect = if_else(!(week %in% testWeeks), NA, 
                                  if_else(coverPred == cover, 1, 0))) %>% 
    relocate(pointsDiffPredFinal, .after = pointsDiffPred) %>% 
    relocate(winPred, .after = win) %>% 
    relocate(coverPred, .after = cover) %>% 
    select(!matches("X"))

# Add top-10 cover performers simulation to this data:
performers_sim <-
  df4 %>% 
  group_by(school) %>% 
  arrange(week) %>% 
  filter(week %in% testWeeks) %>% 
  mutate(cumCorrect = cummean(coverCorrect)*row_number(),
         propCorrect = cummean(coverCorrect),
         totalGames = row_number()) %>% 
  select(school, week, coverPred, coverCorrect, cumCorrect, propCorrect, totalGames)  %>% 
  ungroup() %>%
  group_by(week) %>%  
  arrange(week, desc(propCorrect), desc(totalGames)) %>%
  mutate(top10 =if_else(row_number() <= 10, 1, 0),
         position = row_number()) %>% 
  group_by(school) %>% 
  mutate(prevTop10 = lag(top10)) %>% 
  ungroup()

# Print conditional mean correct bets:
performers_sim %>% 
  filter(week >= min(testWeeks) + 1) %>% 
  group_by(prevTop10, coverPred) %>% 
  summarise(propCorrect = mean(coverCorrect, na.rm = T), 
            countCorrect = propCorrect*n(), 
            n = n()) %>% 
  ungroup() %>% 
  filter(!is.na(prevTop10)) %>% 
  mutate(totalCorrect = sum(countCorrect),
         expected = .5*n, 
         chi = sum((countCorrect-expected)^2/expected),
         p = 1-pchisq(chi, df = sum(!is.na(prevTop10))-1)) %>%
  print(n = nrow(.))

# Save Data:
write.csv(df4, paste("downstream/gamesModifiedModel", year, ".csv", sep = "")) 

# Save Data with select variables:
df5 <- df4 %>% select(!matches("Diff")) %>% ungroup()
write.csv(df5, paste("downstream/gamesModifiedModel", year, "Condensed.csv", sep = ""))
write.csv(df5, paste("cfbapp/gamesModifiedModel", year, "Condensed.csv", sep = ""))

# Clear Memory
rm(list = setdiff(c("year", "runnerPath", "testWeeks", "df4"), ls()))
gc()
