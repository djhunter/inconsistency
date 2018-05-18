# This script will create two plots (each showing RHH and LHH) for the game whose id is in gid
# The first plot shows the alpha-convex hull method.
# The second plot shows the rectangle method.

# Some sample game id's:
#gid <- "gid_2017_08_12_chnmlb_arimlb_1"
#gid <- "gid_2017_08_20_miamlb_nynmlb_1"
#gid <- "gid_2017_08_12_chnmlb_arimlb_1" # not great game
#gid <- "gid_2017_07_01_bosmlb_tormlb_1" # well-called game
#gid <- "gid_2017_07_04_anamlb_minmlb_1"
#gid <- "gid_2017_07_05_nynmlb_wasmlb_1"
#gid <- "gid_2017_08_10_kcamlb_slnmlb_1"
#gid <- "gid_2017_04_20_wasmlb_atlmlb_1" # only 30 calls
#gid <- "gid_2017_06_04_chamlb_detmlb_1" # worst game by metric
#gid <- "gid_2017_04_14_pitmlb_chnmlb_1" # balanced on each side
#gid <- "gid_2017_05_12_cinmlb_sfnmlb_1"
#gid <- "gid_2017_08_27_detmlb_chamlb_1" # less than 3 strikes
#gid <- "gid_2017_09_15_slnmlb_chnmlb_1"
#gid <- "gid_2017_09_16_slnmlb_chnmlb_1"
gid <- "gid_2017_08_20_slnmlb_pitmlb_1" # all pitches are NA's

library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

library(alphahull)
source('inconidx.R')
source('inconrect.R')

library(scales) # for transparency

# Set parameters for alpha-convex hull
alp <- NULL # Search for optimal alpha
alp <- 0.6
alp_rat = 0.95
# Set parameter for rectangle method
ly <- 8

inconList <- inconidx(gid, alpha=alp, alpha_ratio = alp_rat)
ilr <- inconRect(gid, layers = ly)

par(mfrow=c(1,2)) # plot side by side
ballsize=1.5
transp=0.6
x_range <- c(-2.3,2.3)
y_range <- c(0,4.5)

pitchdata <- subset(pitches, gameday_link == gid)
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

emptyhull <- ahull(c(-100, -101, -100), c(0,0,1), alpha=100000) # kludge: not really empty but no data will ever be in it
dummyframe <- data.frame(px=100, normedpz=100) ## for starting an empty plot
plot(dummyframe,xlim=x_range, ylim=y_range, asp=1, col=alpha("blue", transp), pch=19, cex=ballsize)

if (nrow(Rballs)>2) RballHull <- ahull(Rballs, alpha=inconList$alpha_RH) else RballHull <- emptyhull
if (nrow(Rstrikes)>2) RstrikeHull <- ahull(Rstrikes, alpha=10000) else RstrikeHull <- emptyhull

plot(RballHull, add=TRUE, wpoints=FALSE, col=c(4,0,0,0,0,0))
plot(RstrikeHull, add=TRUE, wpoints=FALSE, col=c(2,0,0,0,0,0))
points(Rballs,col=alpha("blue", transp), pch=19, cex=ballsize)
points(Rstrikes, col=alpha("red", transp), pch=19, cex=ballsize)
title(main=paste0(gid, "\nvs. right-handed batters", collapse=""))

plot(dummyframe,xlim=x_range, ylim=y_range, asp=1, col=alpha("blue", transp), pch=19, cex=ballsize)

if (nrow(Lballs)>2) LballHull <- ahull(Lballs, alpha=inconList$alpha_LH) else LballHull <- emptyhull
if (nrow(Lstrikes)>2) LstrikeHull <- ahull(Lstrikes, alpha=10000) else LstrikeHull <- emptyhull

plot(LballHull, add=TRUE, wpoints=FALSE, col=c(4,0,0,0,0,0))
plot(LstrikeHull, add=TRUE, wpoints=FALSE, col=c(2,0,0,0,0,0))
points(Lballs,col=alpha("blue", transp), pch=19, cex=ballsize)
points(Lstrikes, col=alpha("red", transp), pch=19, cex=ballsize)
title(main=paste0(gid, "\nvs. left-handed batters", collapse=""))

plot(dummyframe,xlim=x_range, ylim=y_range, asp=1, col=alpha("blue", transp), pch=19, cex=ballsize)
rect(ilr$M_rhh[,"xmin_r"], ilr$M_rhh[,"ymin_r"], ilr$M_rhh[,"xmax_r"], ilr$M_rhh[,"ymax_r"])
points(Rballs,col=alpha("blue", transp), pch=19, cex=ballsize)
points(Rstrikes, col=alpha("red", transp), pch=19, cex=ballsize)
title(main=paste0(gid, "\nvs. right-handed batters", collapse=""))

plot(dummyframe,xlim=x_range, ylim=y_range, asp=1, col=alpha("blue", transp), pch=19, cex=ballsize)
rect(ilr$M_lhh[,"xmin_l"], ilr$M_lhh[,"ymin_l"], ilr$M_lhh[,"xmax_l"], ilr$M_lhh[,"ymax_l"])
points(Lballs,col=alpha("blue", transp), pch=19, cex=ballsize)
points(Lstrikes, col=alpha("red", transp), pch=19, cex=ballsize)
title(main=paste0(gid, "\nvs. left-handed batters", collapse=""))
