library(baseballr)
library(tidyverse)

first_day_of_season <- ymd("2023-03-31")
game_dates <- seq(from = first_day_of_season, to = today(), by = "day")

game_pks <- map(game_dates, get_game_pks_mlb, level_ids = 11) %>%
  bind_rows()

nonempty_pks <- game_pks %>%
  filter(status.detailedState %in% c("Final", "Completed Early")) %>%
  select(game_pk) %>%
  as_vector()

all_game_pbp_list <- map(nonempty_pks, get_pbp_mlb)
saveRDS(all_game_pbp_list, "aaa/allAAAgames2023.rds")
# Get one game like this
#games <- get_game_pks_mlb(date = c("2023-08-13"), level_ids = 11)
#payload <- get_pbp_mlb(722976)

### Start here if you already have the data:
library(baseballr)
library(tidyverse)
all_game_pbp_list <- readRDS("aaa/allAAAgames2023.rds")
all_game_pbp <- bind_rows(all_game_pbp_list)

# normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
calledPitches <- all_game_pbp %>%
  filter(details.code %in% c("B", "*B", "C"),
         !(is.na(pitchData.coordinates.pX) | is.na(pitchData.coordinates.pZ))) %>%
  transmute(px = pitchData.coordinates.pX, 
            pz = 2.0*(pitchData.coordinates.pZ - pitchData.strikeZoneTop)/
              (pitchData.strikeZoneTop - pitchData.strikeZoneBottom) + 3.5,
            des = details.description,
            stand = matchup.batSide.code) 
