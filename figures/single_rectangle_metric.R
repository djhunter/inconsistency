# Figure ??: One rectangle metric
gid <- "gid_2017_04_14_pitmlb_chnmlb_1" # balanced on each side

library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("../pitches2017.Rda"))

source('~/inconsistency/inconrect.R')

library(scales) # for transparency

# Set parameter for rectangle method
ly <- 1

ilr <- inconRect(gid, layers = ly)

#setEPS()
#cairo_ps(file="~/inconsistency/figures/single_rectangle.eps", 
#         width=16, height=8, onefile = FALSE, 
#         fallback_resolution = 600)

pdf(file="~/inconsistency/figures/single_rectangle.pdf",
    width=9, height=5, paper="special")

par(mfrow=c(1,2)) # plot side by side
ballsize=1.3
transp=0.6
x_range <- c(-2.0,2.0)
y_range <- c(0.5,4.5)

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

dummyframe <- data.frame(px=100, normed_pz=100) ## for starting an empty plot

plot(dummyframe,xlim=x_range, ylim=y_range, asp=1, col=alpha("blue", transp), pch=19, cex=ballsize)
points(Lballs,col=alpha("blue", transp), pch=19, cex=ballsize)
points(Lstrikes, col=alpha("red", transp), pch=19, cex=ballsize)
rect(ilr$M_lhh[,"xmin_l"], ilr$M_lhh[,"ymin_l"], ilr$M_lhh[,"xmax_l"], ilr$M_lhh[,"ymax_l"])
title(main=paste0(gid, "\nvs. left-handed batters", collapse=""))

plot(dummyframe,xlim=x_range, ylim=y_range, asp=1, col=alpha("blue", transp), pch=19, cex=ballsize)
points(Rballs,col=alpha("blue", transp), pch=19, cex=ballsize)
points(Rstrikes, col=alpha("red", transp), pch=19, cex=ballsize)
rect(ilr$M_rhh[,"xmin_r"], ilr$M_rhh[,"ymin_r"], ilr$M_rhh[,"xmax_r"], ilr$M_rhh[,"ymax_r"])
title(main=paste0(gid, "\nvs. right-handed batters", collapse=""))

par(mfrow=c(1,1)) # reset plot parameters

dev.off()