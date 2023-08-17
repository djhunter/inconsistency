library(baseballr)
library(tidyverse)

first_day_of_season <- ymd("2023-03-31")
last_day <- ymd("2023-08-16")
game_dates <- seq(from = first_day_of_season, to = last_day, by = "day")

game_pks <- map(game_dates, get_game_pks_mlb, level_ids = 11) %>%
  bind_rows()

nonempty_pks <- game_pks %>%
  filter(status.detailedState %in% c("Final", "Completed Early")) %>%
  select(game_pk) %>%
  as_vector()

all_game_pbp_list <- map(nonempty_pks, get_pbp_mlb)
saveRDS(all_game_pbp_list, "aaa/allAAAgames2023.rds")
