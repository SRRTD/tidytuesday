library(magrittr)
library(tidyverse)
library(worldfootballR)

prem_shots <- 
  understat_league_season_shots(league = "EPL", season_start_year = 2024)

chelsea_shots <-
  prem_shots %>%
    filter(home_team == "Chelsea" & h_a == "h" | away_team == "Chelsea" & h_a == "a")

chelsea_ids <- 
  chelsea_shots %>%
  distinct(match_id) %>%
  pull()

match_urls <- 
  map_chr(chelsea_ids, function(id) {
    paste("https://understat.com/match/", id, sep = "") 
    })

chelsea_player_stats <-
  map(match_urls, function(url) { 
        Sys.sleep(5)
        return(understat_match_players(url))
        }) %>%
  bind_rows() %>%
  filter(team_id == 80)

chelsea_joined <- 
  chelsea_shots %>%
    group_by(match_id, home_team, away_team, date) %>%
    nest() %>%
    left_join(chelsea_player_stats %>%
                group_by(match_id) %>%
                nest() %>%
                mutate(match_id = as.character(match_id)),
              join_by(match_id)
    )

chelsea_joined %<>%
  rename(shots = data.x, players = data.y)

chelsea_joined %>%
  write_rds("25W1/DATA/chelsea_data.rds")

