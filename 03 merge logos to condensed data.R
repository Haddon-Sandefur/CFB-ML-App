##################################################
# Purpose:
#
# Merge college team logo info to data for plotting
##################################################

# Data year
year = 2022

# Libraries
library(tidyverse)
library(cfbfastR)

# Assert that the condensed file exists. If it does, that means the model files have been ran.
if(!file.exists(paste("../downstream/gamesModified", year, "Condensed.csv", sep = ""))){
  stop("The required data does not exist. Ensure that scripts in the 'models' folder have been ran in sequence.")
}

# Data 
df <- read.csv(paste("../downstream/gamesModified", year, "Condensed.csv", sep = ""))

# Load team logo info
logos <- cfbfastR::load_cfb_teams(fbs_only = T)
logos <- logos %>% select(-conference)

# Left Join to our data
df2 <- left_join(df, logos, by = "school", relationship = "many-to-one") %>% select(-X, -X.1, -X.2)

# Save data:
write.csv(df2, paste("../downstream/gamesModified", year, "CondensedLogos.csv", sep = ""))

# Clear Memory
rm(list = ls())
