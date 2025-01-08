library(magrittr)
library(tidyverse)
library(gganimate)
library(ggsoccer)

chelsea_data <-
  read_rds("25W1/DATA/chelsea_data.rds") %>%
  mutate(date = ymd_hms(date))

md <-
  chelsea_data %>% 
  select(match_id, date) %>%
  ungroup() %>%
  mutate(md = 1:20) %>%
  select(match_id, md)

chelsea_data %<>%
  left_join(md)

chelsea_players <- 
  chelsea_data %>%
  select(!shots) %>%
  unnest(cols = c(players))

chelsea_players %<>%
  group_by(player) %>%
  nest() %>%
  mutate(data = map(data, function(df) {
    df %>% mutate(
      cumulative_minutes = cumsum(time_played),
      cumulative_goals = cumsum(goals),
      cumulative_shots = cumsum(shots),
      cumulative_xg = cumsum(xG),
      goals_per_90 = (cumulative_goals / cumulative_minutes) * 90,
      shots_per_90 = (cumulative_shots / cumulative_minutes) * 90,
      xg_per_90 = (cumulative_xg / cumulative_minutes) * 90,
      xg_per_shot = cumulative_xg / cumulative_shots
    )})) %>%
  unnest(cols = c(data))


