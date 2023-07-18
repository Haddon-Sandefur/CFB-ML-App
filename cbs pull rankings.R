######################
# Purpose: 
#
# Pull CBS Rankings 
######################
# Library
library(rvest)

# Set working directory to runner path
setwd(runnerPath)

# Pull
link <- "https://www.cbssports.com/college-football/rankings/cbs-sports-rankings/"
html <- rvest::read_html(link)

# Extract Ratings:
cbsRankings      <- rvest::html_table(html)[[1]]
cbsRankings$Team <- gsub("[\n].*", "", cbsRankings$Team)

# Change Column Name
colnames(cbsRankings)[which(colnames(cbsRankings) == "Team")] <- "cbsName"
colnames(cbsRankings)[which(colnames(cbsRankings) == "Rank")] <- "cbsRank"

# Keep only team name and rating:
cbsRankings <- cbsRankings[c("cbsRank", "cbsName")]
Sys.sleep(1)