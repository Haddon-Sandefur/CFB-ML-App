# Set working directory to runner path
setwd(runnerPath)

# Source the cfb week helper
source("helpers/cfb week.R")

# Append all weeks of ESPN game-by-game stats.
games <- list()
for (i in 1:(returnCfbWeek() - 1)) {
  games[[i]] <- cfbfastR::cfbd_game_team_stats(year = year, week = i)
}

games   <- do.call("rbind", games)
games$row_id <- 1:nrow(games)

# Download betting information per game. Use Only one provider:
bets    <- cfbd_betting_lines(year = year) %>%
           filter(provider == "Bovada")

# Merge betting data to game-by-game data.
games   <- left_join(games, bets, by = "game_id", relationship = "many-to-many")

# Not interested in books where the moneyline is not present.
# Can remove these by dropping the below variable.
games   <- games %>% drop_na(away_moneyline)

# Convert columns to camel case. 
games   <- janitor::clean_names(games, "lower_camel")

# Convert "completionAttempts" string to a percentage float.
comp   <- as.numeric(word(games$completionAttempts, 1, sep = "-"))
att    <- as.numeric(word(games$completionAttempts, 2, sep = "-"))
perc   <- comp/att
games  <- games %>% mutate(completionPercent = perc)

# Convert totalPenalytYardsAllowed string to int
penaltyYards    <- as.numeric(word(games$totalPenaltiesYards, 2, sep = "-"))
penaltyYardsOpp <- as.numeric(word(games$totalPenaltiesYardsAllowed, 2, sep = "-"))

games <- games %>% mutate(penaltyYards    = penaltyYards,
                          penaltyYardsOpp = penaltyYardsOpp)

rm(list = c("att", "perc", "penaltyYards", "penaltyYardsOpp"))

# Get CBS Sports CFB Rankings
source("helpers/cbs pull rankings.R")
Sys.sleep(3)

# Merge CBS CFB Rankings to Team Info Data
# Download Team Info Data:
dft      <- cfbfastR::load_cfb_teams() %>% select(school, alt_name3)
cbsTeams <- read.table("downstream/cbsTeamXwalk2023.txt", sep = ",") 
teamX    <- left_join(dft,   cbsTeams, by = "alt_name3",  relationship = "one-to-one")
teamX    <- left_join(teamX, cbsRankings, by = "cbsName", relationship = "one-to-one") %>% select(-alt_name3, -cbsName)

# Merge Rankings to games:
games <- left_join(games, teamX, by = "school", relationship = "many-to-one")

# Rename teamX's "school" to "opponent", "cbsRank" to "cbsRankOpp" to bring in Opponent rankings
colnames(teamX) <- c("opponent", "cbsRankOpp")
games           <- left_join(games, teamX, by = "opponent", relationship = "many-to-one")

# Add Conference Average CBS Ranking
games <- games %>% group_by(conference) %>% mutate(cbsConfRank = mean(cbsRank, na.rm = T))

# Add Opponent Conference Average CBS Ranking
games <- games %>% group_by(opponentConference) %>% mutate(cbsConfRankOpp = mean(cbsRankOpp, na.rm = T))

# Save data
write.csv(games, paste("downstream/games", year, ".csv", sep = ""), row.names = FALSE)

# Clear Memory
rm(list = c("games", "dft", "cbsTeams", "teamX"))
Sys.sleep(3)
