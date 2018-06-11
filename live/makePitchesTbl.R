# populate the variable "pitches" so that the scripts will work
library(dplyr)
library(tibble)

# If you want the 2017 season, just do this:
# pitches <- as_data_frame(readRDS("pitches2017.Rda"))

library(mlbgameday)
# if you want all games for a range of dates, try this:
# start <- "2018-06-09"
# end <- "2018-06-09"
# gamedata <- get_payload(start = start, end = end)
# gameIDs <- unique(gamedata$atbat$gameday_link)

# if you know what game you want, do this:
gameIDs <- "gid_2018_06_10_pitmlb_chnmlb_1"
gamedata <- get_payload(game_ids = gameIDs)

library(pitchRx)
playerdata <- scrape(game.ids = gameIDs, suffix = "players.xml")
pitches <- inner_join(gamedata$pitch, gamedata$atbat, by = c("num", "gameday_link"))
hpus <- subset(playerdata$umpire, position=="home")[, c("name", "id", "gameday_link")]
names(hpus) <- c("umpName", "umpID", "gameday_link")
pitches <- inner_join(pitches, hpus, by="gameday_link")

