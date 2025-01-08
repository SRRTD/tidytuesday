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
  unnest(cols = c(data)) %>%
  ungroup()

#Turns out patchwork doesn't work with gganimate

# goalscorers <-
#   chelsea_players %>%
#     select(md, player, goals, cumulative_goals) %>%
#     group_by(md) %>%
#     mutate(rank = rank(-cumulative_goals, ties.method = "first")) %>%
#     arrange(md) %>%
#     #filter(md == 15) %>%
#     ggplot(aes(x = rank, y = cumulative_goals)) +
#     geom_col(fill = "#034694", width = 0.5) +
#     geom_text(aes(y = cumulative_goals, label = player , hjust = -0.05), size = 7) +
#     geom_text(aes(y = cumulative_goals, label = cumulative_goals, hjust = 3), size = 8, color = "white") +
#     coord_flip() +
#     xlim(5.5, 0.5) +
#     ylim(0, 20) +
#     labs(title = "Main goalscorers",
#          x = NULL,
#          y = "Cumulative goals") +
#     theme_minimal() +
#     theme(
#       axis.text.y = element_blank(),
#       axis.ticks.y = element_blank(),
#       plot.margin = margin(20, 20, 20, 20),
#       panel.grid.major.y = element_blank(),
#       panel.grid.minor.y = element_blank(),
#       plot.title = element_text(size = 30, hjust = 0.5, vjust = 3),
#       axis.text.x = element_text(size = 15),
#       axis.title.x = element_text(size = 17, vjust = -2)) +
#       transition_states(md, state_length = 10, transition_length = 0, wrap = FALSE)
#   
#   

