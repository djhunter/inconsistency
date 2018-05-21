# Figure ??: Ten rectangle metric
gid <- "gid_2017_04_14_pitmlb_chnmlb_1" # balanced on each side

library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

source('inconrect.R')
source('figures/prettyGID.R')

library(ggplot2)
library(scales) # for transparency
library(gridExtra) # for more than one plot in a figure

# Set parameter for rectangle method
ly <- 10

ilr <- inconRect(gid, layers = ly)

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
lrects <- data.frame(xmin=ilr$M_lhh[,"xmin_l"], ymin=ilr$M_lhh[,"ymin_l"], 
                     xmax=ilr$M_lhh[,"xmax_l"], ymax=ilr$M_lhh[,"ymax_l"])
rrects <- data.frame(xmin=ilr$M_rhh[,"xmin_r"], ymin=ilr$M_rhh[,"ymin_r"], 
                     xmax=ilr$M_rhh[,"xmax_r"], ymax=ilr$M_rhh[,"ymax_r"])
srlplot <- ggplot() + 
           geom_point(data=Lballs, aes(x=px,y=pz), alpha=0.3, color="blue", size=3, stroke=1) +
           geom_point(data=Lstrikes, aes(x=px,y=pz), alpha=0.3, color="red3", size=3, stroke=1, shape=23, fill="red3") +
           geom_rect(data=lrects, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), 
                     color="red", fill=NA, linetype="solid", size=0.3) +
           coord_fixed(xlim=c(-1.7,1.7), ylim=c(0.8,4.2)) + 
           theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
           ggtitle(prettyGID(gid), subtitle="vs. left-handed batters")
srrplot <- ggplot() + 
           geom_point(data=Rballs, aes(x=px,y=pz), alpha=0.3, color="blue", size=3, stroke=1) +
           geom_point(data=Rstrikes, aes(x=px,y=pz), alpha=0.3, color="red3", size=3, stroke=1, shape=23, fill="red3") +
           geom_rect(data=rrects, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), 
                     color="red", fill=NA, linetype="solid", size=0.3) +
           coord_fixed(xlim=c(-1.7,1.7), ylim=c(0.8,4.2)) + 
           theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
           ggtitle(" ", subtitle="vs. right-handed batters")
ggsave("figures/ten_rect_metric.pdf", plot = grid.arrange(srlplot, srrplot, ncol=2), 
       width = 7, height = 4, dpi = 300)