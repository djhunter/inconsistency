# This script will produce a data frame containing the results of several metrics on each game

library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

library(alphahull)
source('inconidx.R')
source('inconrect.R')

gid <- unique(pitches$gameday_link)
numgames <- length(gid)

incR1 <- numeric(numgames)
incR3 <- numeric(numgames)
incR5 <- numeric(numgames)
incR10 <- numeric(numgames)
incR11 <- numeric(numgames)
incH4 <- numeric(numgames)
incH5 <- numeric(numgames)
incH6 <- numeric(numgames)
incH7 <- numeric(numgames)
incH8 <- numeric(numgames)
npitch <- numeric(numgames)
umpname <- character(numgames)
umpid <- numeric(numgames)

for(i in 1:numgames) {
#for(i in 1:10) {
  pitchdata <- subset(pitches, gameday_link == gid[i])
  calledPitches <- pitchdata[pitchdata$des=="Ball" | 
                             pitchdata$des=="Ball In Dirt" | 
                             pitchdata$des=="Called Strike", c("px","pz")]
  calledPitches <- calledPitches[!is.na(calledPitches[,1]),]
  npitch[i] <- nrow(calledPitches)
  if(npitch[i] > 0) {
    incR1[i] <- inconRect(gid[i], layers = 1)$incR 
    incR3[i] <- inconRect(gid[i], layers = 3)$incR 
    incR5[i] <- inconRect(gid[i], layers = 5)$incR 
    incR10[i] <- inconRect(gid[i], layers = 10)$incR 
    incR11[i] <- inconRect(gid[i], layers = 11)$incR 
    incH4[i] <- inconidx(gid[i], alpha=0.4)$incIdx 
    incH5[i] <- inconidx(gid[i], alpha=0.5)$incIdx 
    incH6[i] <- inconidx(gid[i], alpha=0.6)$incIdx 
    incH7[i] <- inconidx(gid[i], alpha=0.7)$incIdx 
    incH8[i] <- inconidx(gid[i], alpha=0.8)$incIdx 
  }
  umpname[i] <- as.character(pitchdata$umpName[1])
  umpid[i] <- pitchdata$umpID[1]
  if((i %% 10) == 0) cat(".")
  if((i %% 100) == 0) cat(" Processed", i, "games.\n")
}

games17 <- data.frame(gid, umpname, umpid, npitch, incR1, incR3, incR5, incR10, incR11, 
                      incH4, incH5, incH6, incH7, incH8)
saveRDS(games17, file="games17.Rda")

#pairs(games17[,c(5,7,8,9,10,11,12,13)], pch=".", upper.panel=panel.smooth)

