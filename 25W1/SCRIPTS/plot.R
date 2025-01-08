library(magrittr)
library(tidyverse)
library(gganimate)
library(ggsoccer)

chelsea_data <-
  read_rds("25W1/DATA/chelsea_data.rds") %>%
  mutate(date = ymd_hms(date))

chelsea_players <- 
  chelsea_data %>%
  select(!shots) %>%
  unnest(cols = c(players)) %>%
  ungroup()

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

chelsea_players %>%
  ungroup() %>%
  filter(md %in% 1:20) %>%
  group_by(md) %>%
  arrange(desc(cumulative_goals)) %>%
  #slice_head(n = 5) %>%
  ggplot(aes(x = cumulative_goals, y = reorder(player, cumulative_goals))) +
  geom_col() +
  labs(title = 'Match Day: {closest_state}', x = 'Cumulative Goals', y = 'Player') + 
  transition_states(md, transition_length = 2, state_length = 1) + 
  ease_aes('cubic-in-out')

