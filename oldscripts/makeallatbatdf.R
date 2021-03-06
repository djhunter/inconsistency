alldata <- readRDS("all2017gamedata.Rda")
library(plyr)
n_games <- nrow(alldata)
#n_pitches <- 723687
abpl <- list()
k=1
for (i in 1:n_games) {
    playerdata <- alldata[i,]$player[[1]]
    gamedata <- alldata[i,]$game[[1]]
    pitchdata <- gamedata$pitch # all pitches for game
    atbatdata <- gamedata$atbat # all batter info for game
    n_pitchesingame <- nrow(pitchdata)
    for (j in 1:n_pitchesingame) {
      abpl[[k]] <- atbatdata[atbatdata$num==pitchdata[j,]$num,]
      k = k + 1
    }
    if((i %% 10) == 0) cat(".")
    if((i %% 100) == 0) cat(" Processed", i, "games.\n")
}
cat(" Finished constructing list.\n")
atbatbypitchdf <- rbind.fill(abpl)
saveRDS(atbatbypitchdf, "atbatpitchdf2017.Rda")