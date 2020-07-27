# Can we reproduce the results in the widely-publicized BU study:
# http://www.bu.edu/articles/2019/mlb-umpires-strike-zone-accuracy/

## Study claims: MLB Umpires Missed 34,294 Ball-Strike Calls in 2018. 

library(dplyr)
library(tibble)
library(rgeos)
library(lubridate)
pitches <- as_tibble(readRDS("pitches2015-2018.Rda")) %>%
  filter(year(date) == 2018)

# Rule book zone: up/down pz's have been normalized to go from
# 1.5 to 3.5. Width of baseball is 0.245 feet, so we add 1/2 of
# a baseball's width to each edge. Width of plate is 17 inches.
# (17/12)/2+0.245/2 = 0.8308333
hbb <-  0.245/2 ## 33038 missed calls

## To reproduce the BU results, we need to change the size of the baseball.
hbb <- 0 ## 43203 missed calls
hbb <- 0.1 ## 33919 missed
hbb <- 0.05 ## 37573 missed
hbb <- 0.07 ## 35799 missed
hbb <- 0.08 ## 35094 missed
hbb <- 0.09 ## 34440 missed
hbb <- 0.094 ## 34225 missed
hbb <- 0.093 ## 34290 missed
hbb <- 0.0929 ## 34297 missed
hbb <- 0.09291 ## 34296 missed
hbb <- 0.09293 ## 34294 missed <<<<----------!!!!!
rbzoneX <- c(-(17/24 + hbb), (17/24 + hbb), (17/24 + hbb), -(17/24 + hbb))
rbzoneY <- c(1.5 - hbb, 1.5 - hbb, 3.5 + hbb, 3.5 + hbb)
pitchdata <- pitches
# normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
pitchdata$pz <- 2.0*(pitchdata$pz-pitchdata$sz_top)/(pitchdata$sz_top-pitchdata$sz_bot)+3.5
calledPitches <- pitchdata[pitchdata$des=="Ball" | 
                           pitchdata$des=="Ball In Dirt" | 
                           pitchdata$des=="Called Strike", c("px","pz","des","stand")]
calledPitches <- calledPitches[!is.na(calledPitches$px),]
npitch <- nrow(calledPitches)
balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]
accRB <- (sum(point.in.polygon(strikes$px, strikes$pz, rbzoneX, rbzoneY)) +
            nrow(balls) - sum(point.in.polygon(balls$px, balls$pz, rbzoneX, rbzoneY))) /
            npitch
numMissedCalls <- (1-accRB) * npitch

# We can get the same numbers if we assume the diameter of a baseball is about 2.23 inches.
