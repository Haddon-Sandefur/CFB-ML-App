library(tidyverse)
library(cfbfastR)
library(cfbplotR)
library(gt)
library(fontawesome)

# REQUIRED:
source("helpers/cfb week.R")

# Get Lines
schedule <- cfbfastR::cfbd_betting_lines(year = 2023, week = returnCfbWeek())

# Duplicate team cols and subset:
schedule <- 
  schedule %>% 
    filter(provider == "Bovada") %>% 
    select(matches("home"), matches("away"), -matches("conference"), formatted_spread, over_under)

schedule <- 
  schedule %>% 
    gt() %>%
    tab_options(table.background.color = "transparent") %>% 
    tab_footnote("Book info provided by Bovada") %>% 
    tab_style(style = cell_text(weight = "bold"),
              locations = list(cells_column_labels(), cells_title())) %>%
    tab_style(style = cell_text(
      size = "medium",
      weight = "bold",
      ),
      locations = cells_body(
        columns = c(formatted_spread, over_under)
      )) %>% 
    tab_style(style = cell_text(
      size = "large",
      weight = "bold",
      style = "italic",
      decorate = "underline"
    ),
    locations = cells_body(
      columns = c(home_score, away_score)
    )) %>% 
    tab_options( data_row.padding = px(10)) %>% 
    cols_add(home_win = if_else(is.na(home_score), "white",
                            if_else(home_score > away_score, "forestgreen", "white"))
             ) %>%
    cols_add(away_win = if_else(is.na(away_score), "white",
                              if_else(home_score < away_score, "forestgreen", "white"))
    ) %>% 
    cols_move(home_win, after = home_score) %>% 
    cols_move(away_win, after = away_score) %>% 
    text_case_match("forestgreen" ~ fontawesome::fa("check", width = "1.5em")) %>% 
    text_case_match("white" ~ "") %>% 
    tab_style(
      style = cell_text(color = from_column("home_win")),
      locations = cells_body(columns = home_win)
    ) %>% 
    tab_style(
      style = cell_text(color = from_column("away_win")),
      locations = cells_body(columns = away_win)
    ) %>% 
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "black",
        weight = px(3)
      ),
      locations = cells_body()
    ) %>% 
    data_color(
      columns = c(home_moneyline, away_moneyline),
      method = "numeric",
      fn = scales::col_bin(
        bin = c(-Inf, -1000, -500, -250, -100, 0,100, 250, 500, 1000, Inf),
        palette = "RdYlGn",
        reverse = TRUE
      )
    ) %>% 
    cols_label(home_team = "Home Team",
               home_score = "Home Score",
               home_moneyline = "Home Moneyline",
               away_team = "Away Team",
               away_score = "Away Score",
               away_moneyline = "Away Moneyline",
               formatted_spread = "Spread",
               over_under = "O/U",
               home_win = "",
               away_win = "") %>% 
    gt_fmt_cfb_wordmark(columns = "home_team") %>% 
    gt_fmt_cfb_wordmark(columns = "away_team") 
  





