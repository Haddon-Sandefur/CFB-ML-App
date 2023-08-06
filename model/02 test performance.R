# Purpose:
# Test Prediction Perfomance and modify predictions to be symmetric per game

# Libraries
library(tidyverse)
library(tidymodels)

# Data
df <- read.csv("../downstream/gamesModified2022.csv")

# Test Var
df$test <- df$pointsDiffPred - df$spreadOpen/2 + .1*df$pointsAvgLagDiff

# RMSE
rmse(df, pointsDiff, test)
rmse(df%>% filter(week >8), pointsDiff, spreadInverted)
rmse(df, pointsDiff, pointsDiffPred)

# Make symmetric
df2 <- df %>% 
       group_by(gameId) %>%
       mutate(helper  = abs(test),
              helper2 = round(mean(helper)),
              symTest = if_else(win == 1, helper2, -helper2)) %>% 
       ungroup()
     
# Wow... beats the spread prediction by 2.2 points on average
rmse(df2 %>% filter(week >8), pointsDiff, symTest)

# Quick Spread coverage evaluator
df3 <- df2 %>% 
       mutate(symTestInverted = -symTest,
              coverPred = if_else(symTestInverted >= spreadOpen, 0, 1)) %>% 
       drop_na(coverPred) %>% 
       mutate(coverPred = as.integer(coverPred))

df3 %>% select(school, opponent, pointsDiff, symTest, spreadOpen, symTestInverted, cover, coverPred) %>% filter(school == "Duke") %>% view()

caret::confusionMatrix(as.factor(df3$coverPred), as.factor(df3$cover), positive = "1")
