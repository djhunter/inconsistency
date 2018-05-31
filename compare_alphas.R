# This script will produce a data frame containing the results of several metrics on each game

library(dplyr)
library(tibble)
pitches <- as_data_frame(readRDS("pitches2017.Rda"))

library(alphahull)
source('inconhulls.R') # index using area overlap
source('inconconvex.R') # index using convex hull only

gid <- unique(pitches$gameday_link)
numgames <- length(gid)
npitch <- numeric(numgames)
umpname <- character(numgames)
umpid <- numeric(numgames)

# Inconsistency metrics
incCH <- numeric(numgames)
incACH4 <- numeric(numgames)
incACH5 <- numeric(numgames)
incACH6 <- numeric(numgames)
incACH7 <- numeric(numgames)
incACH8 <- numeric(numgames)
incACH9 <- numeric(numgames)

for(i in 1:numgames) {
# for(i in 1:10) { # for testing purposes
  pitchdata <- subset(pitches, gameday_link == gid[i])
  calledPitches <- pitchdata[pitchdata$des=="Ball" | 
                             pitchdata$des=="Ball In Dirt" | 
                             pitchdata$des=="Called Strike", c("px","pz")]
  calledPitches <- calledPitches[!is.na(calledPitches[,1]),]
  npitch[i] <- nrow(calledPitches)
  if(npitch[i] > 0) {
    incCH[i] <- inconConvex(gid[i])
    incACH4[i] <- inconAlphaConvex(gid[i], alpha=0.4)$incACH
    incACH5[i] <- inconAlphaConvex(gid[i], alpha=0.5)$incACH
    incACH6[i] <- inconAlphaConvex(gid[i], alpha=0.6)$incACH
    incACH7[i] <- inconAlphaConvex(gid[i], alpha=0.7)$incACH
    incACH8[i] <- inconAlphaConvex(gid[i], alpha=0.8)$incACH
    incACH9[i] <- inconAlphaConvex(gid[i], alpha=0.9)$incACH
  }
  umpname[i] <- as.character(pitchdata$umpName[1])
  umpid[i] <- pitchdata$umpID[1]
  if((i %% 10) == 0) cat(".")
  if((i %% 100) == 0) cat(" Processed", i, "games.\n")
}
cat("Done.\n")

games17alphas <- data.frame(gid, umpname, umpid, npitch, incCH, incACH4, incACH5, incACH6, incACH7, incACH8, incACH9) 
saveRDS(games17alphas, file="compare_alpha.Rda")

