# This script will produce a data frame containing the results of several metrics on each game

library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

library(alphahull)
source('inconidx.R') # original inconsistency index
source('inconhulls.R') # index using area overlap
source('inconconvex.R') # index using convex hull only
source('inconrect.R') # index using rectangles

gid <- unique(pitches$gameday_link)
numgames <- length(gid)
npitch <- numeric(numgames)
umpname <- character(numgames)
umpid <- numeric(numgames)

# Inconsistency metrics
incR1 <- numeric(numgames)
incR10 <- numeric(numgames)
incIDX7 <- numeric(numgames)
incCH <- numeric(numgames)
incACH7 <- numeric(numgames)

for(i in 1:numgames) {
# for(i in 1:10) { # for testing purposes
  pitchdata <- subset(pitches, gameday_link == gid[i])
  calledPitches <- pitchdata[pitchdata$des=="Ball" | 
                             pitchdata$des=="Ball In Dirt" | 
                             pitchdata$des=="Called Strike", c("px","pz")]
  # calledPitches <- calledPitches[!is.na(calledPitches[,1]),] # deprecated
  calledPitches <- calledPitches[!is.na(calledPitches$px),]
  npitch[i] <- nrow(calledPitches)
  if(npitch[i] > 0) {
    incR1[i] <- inconRect(gid[i], layers = 1)$incR 
    incR10[i] <- inconRect(gid[i], layers = 10)$incR 
    incCH[i] <- inconConvex(gid[i])
    incIDX7[i] <- inconidx(gid[i], alpha=0.7)$incIdx
    incACH7[i] <- inconAlphaConvex(gid[i], alpha=0.7)$incACH
  }
  umpname[i] <- as.character(pitchdata$umpName[1])
  umpid[i] <- pitchdata$umpID[1]
  if((i %% 10) == 0) cat(".")
  if((i %% 100) == 0) cat(" Processed", i, "games.\n")
}

games17inc <- data.frame(gid, umpname, umpid, npitch, incR1, incR10,
                         incIDX7, incCH, incACH7)
#saveRDS(games17inc, file="games17inc.Rda")

#pairs(games17[,c(5,7,8,9,10,11,12,13)], pch=".", upper.panel=panel.smooth)
#pairs(games17[,5:9], pch=".", upper.panel = panel.smooth)
#cor(games17[,5:9])
#library("GGally")
#ggpairs(games17[,5:9])
#prcomp(games17[,5:9], scale=TRUE)

