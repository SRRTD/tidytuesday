library(magrittr)
library(tidyverse)

perfumes <- tidytuesdayR::tt_load(2024, week = 50)[[1]]

perfumes %<>%
  select(Name,
         Brand,)