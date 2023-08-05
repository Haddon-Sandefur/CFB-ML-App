########################
# Purpose:
#
# Fit XGBoost model to 
# predict wins.
########################

# Libraries
library(tidyverse)
library(magrittr)
library(dplyr)
library(xgboost)
library(caret)

library(corrplot)

# Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in data
df <- read.csv("../downstream/gamesModified2022.csv")

# Replace all NA with 0
df <- df %>% mutate_all(~replace(., is.na(.), 0))

# Predict Score Diff - drop anything that artificially enhances this prediction... drop week 1 as well
df <- df %>% filter(week != 1)

# Observe a Correlation matrix:
cormat         <- cor(df %>% select(where(is.numeric)))
pointsCorr     <- data.frame(cormat[,"pointsDiff"])
pointsCorr$var <- rownames(pointsCorr)
colnames(pointsCorr) <- c("corr", "var")

# Analyze Lag Diff variables most correlated with pointsDiff Outcome.
pointsCorr %>% arrange(desc(abs(corr))) %>% filter(grepl("Diff", var)) %>% slice(1:30)

# We'll use the non-lagged analogues of these variables to build the model, then predict using the 
# lag and lag avg correspondent vars to develop conservative win/cover estimates

# Check COVER
cormat         <- cor(df %>% select(where(is.numeric)))
pointsCorr     <- data.frame(cormat[,"cover"])
pointsCorr$var <- rownames(pointsCorr)
colnames(pointsCorr) <- c("corr", "var")
pointsCorr %>% arrange(desc(abs(corr))) %>% filter(!grepl("Lag", var) | grepl("spread", var)) %>% slice(1:25)



pca <- prcomp(df %>% select(where(is.numeric)))
biplot(pca)

# Split train and test set:
testWeeks <- c(11, 12, 13, 14)
dfTrain <- df %>% filter(!(week %in% testWeeks))
dfTest  <- df %>% filter(week %in% testWeeks)

