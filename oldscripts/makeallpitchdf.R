#alldata <- readRDS("all2017gamedata.Rda")
library(plyr)
n_games <- nrow(alldata)
allpitchdf <- alldata[1,]$game[[1]]$pitch # start with first game
n_pitches = nrow(allpitchdf)
for (i in 2:n_games) {
#    playerdata <- alldata[i,]$player[[1]]
    gamedata <- alldata[i,]$game[[1]]
    pitchdata <- gamedata$pitch # all pitches for game
    allpitchdf <- rbind.fill(allpitchdf, pitchdata)
    n_pitches = n_pitches + nrow(pitchdata)
    if((i %% 10) == 0) cat(".")
    if((i %% 100) == 0) cat(" Processed", i, "games.\n")
}
# n_pitches <- 723687
umpnameid <- data.frame(umpName = character(n_pitches), umpID = numeric(n_pitches),
                      stringsAsFactors = FALSE)
k=1
for (i in 1:n_games) {
    playerdata <- alldata[i,]$player[[1]]
    gamedata <- alldata[i,]$game[[1]]
    pitchdata <- gamedata$pitch # all pitches for game
    n_pitchesingame <- nrow(pitchdata)
    unm <- playerdata$umpire[playerdata$umpire$position=="home", "name"]
    umid <- playerdata$umpire[playerdata$umpire$position=="home", "id"]
    for (j in 1:n_pitchesingame) {
      umpnameid[k, "umpName"] <- unm
      umpnameid[k, "umpID"] <- umid
      k = k + 1
    }
    if((i %% 10) == 0) cat(".")
    if((i %% 100) == 0) cat(" Processed", i, "games.\n")
}

allpitchumpdata <- cbind(umpnameid, allpitchdf)