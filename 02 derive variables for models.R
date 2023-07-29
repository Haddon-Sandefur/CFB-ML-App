########################
# Purpose:
#
# Calculate differentials in stats per game
########################

# Library
library(tidyverse)

# Set working directory to runner path
setwd(runnerPath)

# Read in data and sort
games <- read.csv("downstream/games2022.csv") %>% arrange(rowId) 
# NOTE. R v 4.3.1 will coerce certain Latin characters to unicode. Please use R version 4.1.1 (works fine).

games$rowId <- as.character(games$rowId)
games <- games %>% relocate(rowId, .before = gameId )

# Save as "work"
work  <- games

# Replacing NA with zeros everywhere (I believe NAs occur in many situations where events do not occur)
work[is.na(work)] <- 0

# Add some betting information variables: ===============================================================
# Make some mutations regarding the betting lines:
work <- work %>% mutate(favoredTeam = if_else(gsub("\\W*\\d*\\D*\\d$", "", formattedSpread) == school, 1, 0),
                        spread      = if_else(favoredTeam == 1, -abs(spread),     abs(spread)),
                        spreadOpen  = if_else(favoredTeam == 1, -abs(spreadOpen), abs(spreadOpen)),
                        moneyline   = ifelse(school       == homeTeam, homeMoneyline,  awayMoneyline))

# Take Cumulative Means ==================================================================================
workAvg <- work %>% 
  group_by(school) %>% 
  mutate_if(is.numeric, cummean) %>% 
  ungroup()

# Round Everything 
workAvg <- workAvg %>% mutate(across(where(is.numeric), round, 3))
workAvg <- workAvg %>% {bind_cols(select_at(., "rowId"),
                                  select_if(., is.numeric))}

# Rename Colnames with "Avg" suffix at the end
colnames(workAvg)[-1] <- paste(colnames(workAvg)[-1], "Avg", sep = "")

# CBIND Avgs to work by team
work2 <- left_join(work, workAvg, by = "rowId", relationship = "one-to-one")

# Verify integrity of original sort
if(!all(work2$rowId == games$rowId)){
  stop("rowId column not equal to original data... check sort")
}else{
    work2 <- work2 %>% select(-gameIdAvg, -weekAvg)
}

# Lag all numeric variables ==============================================================================
# Lag everything except rowId...keep rowId and numeric results only
workLag <- work2 %>% 
  group_by(school) %>%
  mutate_if(is.numeric, lag) %>% 
  {bind_cols(select_at(., "rowId"),
             select_if(., is.numeric))} %>% 
  ungroup() %>% 
  select(-`school...1`, -`school...3`)

# Change names of columns except rowId
colnames(workLag)[-1] <- paste(colnames(workLag[-1]),"Lag", sep = "")


# Merge back to data
work2 <- left_join(work2, workLag, by = "rowId", relationship = "one-to-one")

# Take Differences ======================================================================================= 
# Group by game and apply differences to numeric variables
work3 <- work2 %>% arrange(gameId) # <- IMPORTANT SORT, DO NOT CHANGE.

workDiff  <- work3 %>%
            {bind_cols(select_at(.,"rowId"),
                       select_if(., is.numeric))}

# Create function for this operation:
diffFunc <- function(data){
  row1 = data[1,] - data[2,]
  row2 = data[2,] - data[1,]
  
  data[1,] <- row1
  data[2,] <- row2
  
  return(data)
}

workDiffTemp <- workDiff %>% select(-rowId)
workDiffKey  <- workDiff %>% select(rowId)

workDiffTemp <- workDiffTemp  %>% 
  group_by(gameId)            %>% 
  group_modify(~ diffFunc(.x))%>%
  ungroup()                   %>%
  arrange(gameId)


# Rename Colnames with "Diff" suffix at the end
colnames(workDiffTemp)[-1] <- paste(colnames(workDiffTemp)[-1], "Diff", sep = "")

# CBIND workDiffTemp and workDiffKeys together overwriting original workDiff
workDiff <- cbind(workDiffKey, workDiffTemp)


# Merge back to working
work3 <- left_join(work3, workDiff, by = "rowId", relationship = "one-to-one") %>% arrange(as.numeric(rowId)) %>% select(-gameId.y) %>% rename(gameId = gameId.x)

# Rename data and clear memory:
final <- work3
rm(list = c("work", "work2", "work3", "workAvg", "workDiff", "workDiffTemp", "workDiffKey", "workLag", "games"))

final$rowId <- as.numeric(final$rowId)

# Add a variable to denote whether a team covered the spread or not:
final <- final %>% mutate(cover = case_when(favoredTeam == 1 & pointsDiff <= -spread ~ 0,
                                   favoredTeam == 1 & pointsDiff >  -spread ~ 1,
                                   favoredTeam == 0 & pointsDiff > 0 ~ 1,
                                   favoredTeam == 0 & pointsDiff > -spread ~ 1,
                                   TRUE ~ 0))

# Add a variable denoting whether a team won or not.
final <- final %>% mutate(win = if_else(pointsDiff > 0, 1, 0))


# Save Master Data
write.csv(final, "downstream/gamesModified2022.csv")

# Save Data for XGBoost Model.
finalXG <- final %>% select(school, opponent, win, cover, points, pointsDiff, week, spread, matches("Lag"), -matches("gameId"), -weekLag, -weekLagDiff)
write.csv(finalXG, "downstream/gamesModifiedXGBoost2022.csv")

# Remove non-used data:
rm(list = c("bets", "cbsRankings", "html"))
