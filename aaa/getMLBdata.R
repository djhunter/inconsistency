library(baseballr)
library(tidyverse)

first_day_of_season <- ymd("2023-03-30")
game_dates <- seq(from = first_day_of_season, to = today(), by = "day")

game_pks <- map(game_dates, get_game_pks_mlb, level_ids = 1) %>%
  bind_rows()

nonempty_pks <- game_pks %>%
  filter(status.detailedState %in% c("Final", "Completed Early")) %>%
  select(game_pk) %>%
  as_vector()

all_game_pbp_list <- map(nonempty_pks, get_pbp_mlb)
saveRDS(all_game_pbp_list, "aaa/allMLBgames2023.rds")
