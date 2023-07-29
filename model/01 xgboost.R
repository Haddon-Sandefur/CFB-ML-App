########################
# Purpose:
#
# Fit XGBoost model to 
# predict wins.
########################

# Libraries
library(xgboost)
library(caret)

# Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in data
df <- read.csv("../downstream/gamesModifiedXGBoost2022.csv")

#