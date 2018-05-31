library(dplyr)
library(tibble)
library(xtable)

games17inc <- as_data_frame(readRDS("games17inc.Rda"))
games17inc <- subset(games17inc, npitch>=50) # throw out games with less than 50 pitches
xtable(cor(games17inc[,c(5,6,8,9)]))
