alldata <- readRDS("all2017gamedata.Rda")
library(plyr)
n_games <- nrow(alldata)
apl <- list()
for (i in 1:n_games) {
    apl[[i]] <- alldata[i,]$game[[1]]$pitch
    if((i %% 10) == 0) cat(".")
    if((i %% 100) == 0) cat(" Processed", i, "games.\n")
}
allpitchdf <- rbind.fill(apl)

cat("Getting umpire names and IDs.\n")
# n_pitches <- 723687
k=1
unil = list()
for (i in 1:n_games) {
    playerdata <- alldata[i,]$player[[1]]
    gamedata <- alldata[i,]$game[[1]]
    pitchdata <- gamedata$pitch # all pitches for game
    n_pitchesingame <- nrow(pitchdata)
    umpName <- playerdata$umpire[playerdata$umpire$position=="home", "name"]
    umpID <- playerdata$umpire[playerdata$umpire$position=="home", "id"]
    for (j in 1:n_pitchesingame) {
      unil[[k]] <- data.frame(umpName=umpName, umpID=umpID)
      k = k + 1
    }
    if((i %% 10) == 0) cat(".")
    if((i %% 100) == 0) cat(" Processed", i, "games.\n")
}

umpnameid <- rbind.fill(unil)
allpitchumpdata <- cbind(umpnameid, allpitchdf)