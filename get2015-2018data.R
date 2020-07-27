# Load data from 90666_1164824_bundle_archive.zip 
# https://www.kaggle.com/pschale/mlb-pitch-data-20152018
# and change format to match pitches2017.Rda

library(dplyr)
library(readr)
library(stringr)
library(magrittr)
library(tibble)
pitches1518 <- read_csv("~/Downloads/mlbdata/pitches.csv",
                    col_types = cols(ab_id = col_character()))
games <- read_csv("~/Downloads/mlbdata/games.csv",
                  col_types = cols(g_id = col_character()))
atbats <- read_csv("~/Downloads/mlbdata/atbats.csv",
                   col_types = cols(ab_id = col_character(),
                                    g_id = col_character()))

# add a gameday link

gameday_link <- games %$% 
  paste0("gid_", str_replace_all(date, "-", "_"), "_", away_team, "mlb_", home_team, "mlb_1")
gameTwos <-  which(c(gameday_link, "xxx") == c("xxx", gameday_link))
str_sub(gameday_link[gameTwos], start = 30, end = 30) <- "2"
games <- add_column(games, gameday_link)

# join all variables together

pitches <- pitches1518 %>%
  mutate(ab_id = substr(ab_id, 1, 10),
         des = case_when(
           code == "B" ~ "Ball",
           code == "*B" ~ "Ball In Dirt",
           code == "C" ~ "Called Strike", 
           code == "F" ~ "Foul",
           code == "X" ~ "In play, out(s)",
           code == "D" ~ "In play, no out",
           code == "S" ~  "Swinging Strike",
           code == "E" ~ "In play, run(s)",
           code == "W" ~ "Swinging Strike (Blocked)",
           code == "T" ~ "Foul Tip",
           code == ""  ~ "Hit By Pitch",
           code == "V" ~ "Automatic Ball",
           code == "L" ~ "Foul Bunt",
           code == "M" ~ "Missed Bunt",
           code == "P" ~ "Pitchout",
           TRUE ~ code
         )) %>%
  left_join(atbats, by = "ab_id") %>%
  left_join(games, by = "g_id") %>%
  mutate(umpName = umpire_HP, # for compatibility
         umpID = umpire_HP,   # for compatibility
         play.guid.1 = ab_id)   # for compatibility

# save result
# saveRDS(pitches, file="pitches2015-2018.Rda")
