library(magrittr)
library(tidyverse)
library(tidytext)
library(patchwork)

data <- tidytuesdayR::tt_load(2025, week = 2)

conf_23 <- data$conf2023

conf_24 <- data$conf2024


twograms_23 <-
  conf_23 %>%
    group_by(track = block_track_title) %>%
    summarise(sessions = str_c(session_abstract, collapse = " ")) %>%
    ungroup() %>%
    unnest_tokens(two_gram, sessions, token = "ngrams", n = 2) %>%
    separate(two_gram, into = c("first_word", "second_word"), sep = " ") %>%
    filter(!first_word %in% stop_words$word & !second_word %in% stop_words$word) %>%
    count(first_word, second_word, sort = TRUE) %>%
    filter(n >= 5)
  

twograms_24 <-
  conf_24 %>%
    group_by(track) %>%
    summarise(sessions = str_c(description, collapse = " ")) %>%
    ungroup() %>%
    unnest_tokens(two_gram, sessions, token = "ngrams", n = 2) %>%
    separate(two_gram, into = c("first_word", "second_word"), sep = " ") %>%
    filter(!first_word %in% stop_words$word & !second_word %in% stop_words$word) %>%
    count(first_word, second_word, sort = TRUE) %>%
    filter(n >= 5)

merged_twograms <-
  twograms_23 %>%
  bind_rows(twograms_24) %>%
  mutate(twogram_join = paste(first_word, second_word, sep = "_"), 
         twogram = paste(first_word, second_word, sep = " ")) %>%
  mutate(twogram_join = if_else(twogram_join == "shiny_apps", "shiny_app", twogram_join)) %>% #shiny app will catch all
  filter(twogram_join != "science_teams") #it comes from the trigram data science teams, so I'll omit it to get data_science and teams separately

two_gram_vec <-
  setNames(merged_twograms$twogram_join, merged_twograms$twogram)
  
conf_23 %<>%
  mutate(session_abstract = str_to_lower(session_abstract), 
         session_abstract = str_replace_all(session_abstract, two_gram_vec))

conf_24 %<>%
  mutate(description = str_to_lower(description), 
         description = str_replace_all(description, two_gram_vec))

conf_23_words <- 
  conf_23 %>%
  distinct(session_title, .keep_all = TRUE) %>%
  group_by(block_track_title) %>%
  summarise(sessions = str_c(session_abstract, collapse = " ")) %>%
  ungroup() %>%
  unnest_tokens(word, sessions) %>%
  anti_join(stop_words %>% filter(!word == "r")) %>%
  count(word, sort = TRUE) %>%
  rename(count_2023 = n) %>%
  filter(word != "talk")

conf_24_words <- 
  conf_24 %>%
    distinct(talk_title, .keep_all = TRUE) %>%
    group_by(track) %>%
    summarise(sessions = str_c(description, collapse = " ")) %>%
    ungroup() %>%
    unnest_tokens(word, sessions) %>%
    anti_join(stop_words %>% filter(!word == "r")) %>%
    count(word, sort = TRUE) %>%
  rename(count_2024 = n) %>%
  filter(word != "talk")

join <-
  conf_23_words %>%
    full_join(conf_24_words, by = "word") %>%
    mutate(count_2023 = ifelse(is.na(count_2023), 0, count_2023), 
           count_2024 = ifelse(is.na(count_2024), 0, count_2024)) %>%
    mutate(diff = count_2024 - count_2023)

#plot_2023 <-
  join %>%
  mutate(word = fct_reorder(word, count_2023)) %>%
  arrange(desc(count_2023)) %>%
  slice_head(n = 6) %>%
  ggplot(aes(count_2023, word)) +
  geom_col() +
  theme_bw() +
  labs(
    x = "Count",
    y = "Token",
    title = "Most common tokens in 2023")) +
  theme(
    
  )
  
  
  join %>%
    mutate(word = fct_reorder(word, count_2024)) %>%
    arrange(desc(count_2024)) %>%
    slice_head(n = 6) %>%
    ggplot(aes(count_2024, word)) +
    geom_col()
  
join %>%
  arrange(diff) %>%
  slice_head(n = 6) %>%
  bind_rows(                         #this is one of the ugliest things i've done
    join %>% arrange(desc(diff)) %>%
      slice_head(n = 6)) %>%
  mutate(word = fct_reorder(word, diff)) %>%
  ggplot(aes(diff, word)) +
  geom_col() +
  xlim(-51, 51)
  