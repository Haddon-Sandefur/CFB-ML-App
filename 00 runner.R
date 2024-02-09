rm(list = ls())
gc()

# Dependencies:
library(tidyverse)
library(cfbfastR)
library(oddsapiR)
library(janitor)
library(stringdist)
library(magrittr)
library(xgboost)
library(tidymodels)
library(Ckmeans.1d.dp)

#Set Directory to script path.
runnerPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(runnerPath)

# Global Variables
year = 2023
runModels = TRUE

# Make downstream directory
if(!dir.exists("downstream")){
    dir.create("downstream")
}

# Make downstream directory and move crosswalk.
# If the file is nowhere to be found - throw an error
# If the file is in the same folder as runner - move it to downstream
# If the file is already in downstream - do nothing
if(!file.exists("cbsTeamXwalk2023.txt")){
  if(!file.exists("downstream/cbsTeamXwalk2023.txt")){
    stop("Please place 'cbsTeamXwalk2023.txt' in the runner.R filepath")
    }
}else{
  if(!file.exists("downstream/cbsTeamXwalk2023.txt")){
    file.copy(from = "cbsTeamXwalk2023.txt", to = "downstream/cbsTeamXwalk2023.txt")
    file.remove("cbsTeamXwalk2023.txt")
  }
}

if(!file.exists("spXwalk.txt")){
  if(!file.exists("downstream/spXwalk.txt")){
    stop("Please place 'spXwalk.txt' in the runner.R filepath")
  }
}else{
  if(!file.exists("downstream/spXwalk.txt")){
    file.copy(from = "spXwalk.txt", to = "downstream/spXwalk.txt")
    file.remove("spXwalk.txt")
  }
}

# Run files.
source("01 get games bets and ranks.R")
source("02 derive variables for models.R")
if(runModels){
  source("model/01 xgb.R")
  source("model/02 adjust prediction and save.R")
}
source("03 merge logos to condensed data.R")