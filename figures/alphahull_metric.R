# Figure ??: alpha-convex hull metric
gid <- "gid_2017_04_14_pitmlb_chnmlb_1" # balanced on each side

library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

source('figures/prettyGID.R')

library(scales) # for transparency

# TODO: write using ggplot instead of regular plot.
library(alphahull)
gameID <- "gid_2017_08_12_chnmlb_arimlb_1"
pitchdata <- subset(pitches, gameday_link == gameID)
# normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
pitchdata$pz <- 2.0*(pitchdata$pz-pitchdata$sz_top)/(pitchdata$sz_top-pitchdata$sz_bot)+3.5
Lpitchdata <- subset(pitchdata, stand=="L")
Rpitchdata <- subset(pitchdata, stand=="R")
Lballs <- Lpitchdata[Lpitchdata$des=="Ball" | Lpitchdata$des=="Ball In Dirt",c("px","pz")]
Lstrikes <- Lpitchdata[Lpitchdata$des=="Called Strike",c("px","pz")]
Rballs <- Rpitchdata[Rpitchdata$des=="Ball" | Rpitchdata$des=="Ball In Dirt",c("px","pz")]
Rstrikes <- Rpitchdata[Rpitchdata$des=="Called Strike",c("px","pz")]
Lballs <- Lballs[!is.na(Lballs[,1]),]
Rballs <- Rballs[!is.na(Rballs[,1]),]
Lstrikes <- Lstrikes[!is.na(Lstrikes[,1]),]
Rstrikes <- Rstrikes[!is.na(Rstrikes[,1]),]
RballHull <- ahull(Rballs, alpha=0.6)
library(ggforce)
rbharcs <- data.frame(x = RballHull$arcs[,"c1"],
                      y = RballHull$arcs[,"c2"], 
                      start = (atan2(RballHull$arcs[,"v.x"], RballHull$arcs[,"v.y"])-RballHull$arcs[,"theta"]), 
                      end = (atan2(RballHull$arcs[,"v.x"], RballHull$arcs[,"v.y"])+RballHull$arcs[,"theta"]),
                      r = RballHull$arcs[,"r"])
ebzplot <- ggplot() + geom_arc(aes(x0=x, y0=y, r=r, start=start, end=end), data=rbharcs)
