#alldata <- readRDS("all2017gamedata.Rda")
n_games <- nrow(alldata)
n_pitches = 0
for (i in 1:n_games) {
    playerdata <- alldata[i,]$player[[1]]
    gamedata <- alldata[i,]$game[[1]]
    pitchdata <- gamedata$pitch # all pitches
    n_pitches = n_pitches + nrow(pitchdata)
}
# n_pitches <- 723687
