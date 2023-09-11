########################
# Purpose:
#
# Fit XGBoost model to 
# predict wins.
########################

# Libraries
library(tidyverse)
library(magrittr)
library(xgboost)
library(tidymodels)

# Data Year
year = year

# Set Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set Seed
set.seed(117)

# Read in data
df <- read.csv(paste("downstream/gamesModified", year,".csv", sep = ""))

# Replace all NA with 0
df2 <- df %>% mutate_all(~replace(., is.na(.), 0))

# Predict Score Diff - Drop Week 1 & grab variables of interest.
df2 <- df2 %>% select(pointsDiff,
                      school,
                      opponent,
                      cbsRankDiff,
                      cbsConfRankDiff,
                      rushTdsAvgLagDiff,
                      rushingYardsAvgLagDiff,
                      totalYardsAvgLagDiff,
                      yardsPerPassAvgLagDiff,
                      penaltyYardsAvgLagDiff,
                      turnoversAvgLagDiff,
                      completionPercentAvgLagDiff,
                      rushTdsLagDiff,
                      rushingYardsLagDiff,
                      totalYardsLagDiff,
                      yardsPerPassLagDiff,
                      penaltyYardsLagDiff,
                      turnoversLagDiff,
                      completionPercentLagDiff,
                      spreadOpen,
                      moneyline,
                      overUnderOpen,
                      week
                      )

# Split train and test set:
testWeeks <- max(df2$week)
dfTrain <- df2 %>% filter(!(week %in% testWeeks)) %>% select(-week)
dfTest  <- df2 %>% filter(week %in% testWeeks)    %>% select(-week)
rm("df2")

# Model Set-Up with Tidymodels =================================================

# Step 1: Preprocess Data
preprocessingRecipe <- 
  recipes::recipe(pointsDiff ~ ., data = dfTrain) %>% # Formula
  recipes::step_string2factor(all_nominal())      %>% # Encode Categorical Variables (Teams)
  recipes::step_nzv(all_nominal())                %>% # Remove no variance Variables (In here just in case)
  prep()

# Step 2: Set CV specifications
cvFolds <- 
  recipes::bake(
    preprocessingRecipe, 
    new_data = dfTrain
  ) %>%  
  rsample::vfold_cv(v = 5)

# Step 3: Initialize XGBoost Model - Fill params with empty tune()
xgbModel <- 
  parsnip::boost_tree(
    mode = "regression",
    trees = 1000,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>%
  set_engine("xgboost", objective = "reg:squarederror")

# Step 4 Grid Search Best Params ------------------------------------------------

## Params we want to search for
xgbParams <- 
  dials::parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )

## Searches per fold
xgbGrid <- 
  dials::grid_max_entropy(
    xgbParams, 
    size = 10
  )

knitr::kable(head(xgbGrid))

## Specify the workflow for the search
xgbWF <- 
  workflows::workflow() %>%
  add_model(xgbModel) %>% 
  add_formula(pointsDiff ~ .)

## Search params - this may take a while but is necessary for automation
xgbTuned <- tune::tune_grid(
  object = xgbWF,
  resamples = cvFolds,
  grid = xgbGrid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = TRUE)
)

xgbTuned %>%
  tune::show_best(metric = "rmse") %>%
  knitr::kable()

## Extract best params
xgbBestParams <- xgbTuned %>%
  tune::select_best("rmse")

knitr::kable(xgbBestParams)

xgbFinal <- xgbModel %>% 
  finalize_model(xgbBestParams)

#---------------------------------------------------------------

# Step 5: Train Model, evaluate Train RMSE
trainProcessed <- bake(preprocessingRecipe,  new_data = dfTrain)

# Create fit.
trPred <- xgbFinal                                             %>%
          fit(formula = pointsDiff ~ ., data = trainProcessed)                              

# Save fit for future predictions.
saveRDS(trPred, "model/xgbModelParsnip.rds")

trPred <- trPred                                               %>% 
          predict(new_data = trainProcessed)                   %>% # Make Predictions
          bind_cols(dfTrain)                                       # Bind processed predictions to og train data.


trPred                                                            %>%
  yardstick::metrics(pointsDiff, .pred)                           %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ",")) %>%
  knitr::kable(format = "pipe", caption = "Train Set")


# Step 6: Test Model, evaluate Test RMSE
testProcessed <- bake(preprocessingRecipe,  new_data = dfTest)

tsPred <- xgbFinal                                             %>%
          fit(formula = pointsDiff ~ ., data = trainProcessed) %>% # Fit Model... again 
          predict(new_data = testProcessed)                    %>% # Make Predictions
          bind_cols(dfTest)                                        # Bind processed predictions to og test data.


tsPred                                                            %>%
  yardstick::metrics(pointsDiff, .pred)                           %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ",")) %>%
  knitr::kable(format = "pipe", caption = "Test Set")

# Step 7: Predict all games and add predictions to original dataset.
dfProcessed <- bake(preprocessingRecipe, new_data = df)

dfPred <- xgbFinal                                             %>%
          fit(formula = pointsDiff ~ ., data = trainProcessed) %>% # Fit Model ... again
          predict(new_data = dfProcessed)                      %>% # Make Predictions
          bind_cols(df)                                            # Bind processed predictions to all data.


dfPred                                                            %>%
  yardstick::metrics(pointsDiff, .pred)                           %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ",")) %>%
  knitr::kable(format = "pipe", caption = "Final Data")

#============================================================================

# Final Steps: Rename some variables and save everything:
dfPred <- dfPred %>%  rename(pointsDiffPred = .pred) %>% relocate(pointsDiffPred, .after = pointsDiff)

# Save Model Specs:
saveRDS(xgbFinal, "xgbModelSpecs.rds")

# Save Data:
write.csv(dfPred, paste("downstream/gamesModifiedModel", year, ".csv", sep = ""))

# Save Processed Training Data for future model interaction
write.csv(trainProcessed, "downstream/trainProcessed.csv")
