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
        return(understat_match_players(url))}) %>%
  bind_rows() %>%
  filter(team_id == 80)

unique_players <- 
  distinct(chelsea_player_stats, player) %>%
  left_join(chelsea_player_stats %>% select(player, player_id) %>% distinct(), 
            by = join_by(player))

chelsea_player_stats_complete <-
  chelsea_player_stats%>%
  group_by(match_id, team_id, home_away) %>%
  nest() %>%
    mutate(data = map(data, function(df) { 
      df %>% right_join(unique_players, by = join_by(player, player_id)) %>% 
        mutate(across(where(is.numeric), 
                      function(x) { 
                        replace_na(x, 0)})) %>% 
        mutate(across(where(is.character), function(x) { 
          replace_na(x, "not_in_squad") 
          })) 
      }))

chelsea_player_stats_complete %<>% unnest(cols = c(data))

chelsea_joined <- 
  chelsea_shots %>%
    group_by(match_id, home_team, away_team, date) %>%
    nest() %>%
    left_join(chelsea_player_stats_complete %>%
                group_by(match_id) %>%
                nest() %>%
                mutate(match_id = as.character(match_id)),
              join_by(match_id)
    )

chelsea_joined %<>%
  rename(shots = data.x, players = data.y)

md <-
  chelsea_data %>% 
  select(match_id, date) %>%
  ungroup() %>%
  mutate(md = 1:20) %>%
  select(match_id, md)

chelsea_joined %<>%
  left_join(md)

chelsea_joined %>%
  write_rds("25W1/DATA/chelsea_data.rds")

