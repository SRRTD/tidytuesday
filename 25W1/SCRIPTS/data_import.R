library(magrittr)
library(tidyverse)
library(worldfootballR)

prem_matches <- 
  fb_match_urls(country = "ENG", gender = "M", season_end_year = 2025, tier = "1st", time_pause = 10)

prem_shots <- 
  understat_league_season_shots(league = "EPL", season_start_year = 2024)

prem_shots_fbr <-
  fb_match_shooting(prem_matches, time_pause = 10)
