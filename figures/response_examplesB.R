# Figure ??: alpha-convex hull metric
gid <- "gid_2017_04_14_pitmlb_chnmlb_1" # balanced on each side

library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

source('figures/prettyGID.R')

library(ggplot2)
library(scales) # for transparency
library(ggforce) # for plotting arcs
library(gridExtra) # for more than one plot in a figure
library(alphahull)
source('inconrect.R')
# Set parameter for rectangle method
ly <- 10

# parameters for alpha-convex hull method
alph <- 0.7

# gameID <- "gid_2017_08_10_balmlb_oakmlb_1" # Figure A
gameID <- "gid_2017_08_13_colmlb_miamlb_1" # Figure B
ilr <- inconRect(gameID, layers = ly)

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
RballHull <- ahull(Rballs, alpha=alph)
RstrikeHull <- ahull(Rstrikes, alpha=10000) 
rshpoints <- data.frame(x = RstrikeHull$xahull[RstrikeHull$arcs[,"end1"],1], 
                        y = RstrikeHull$xahull[RstrikeHull$arcs[,"end1"],2])
rbharcs <- data.frame(x = RballHull$arcs[,"c1"],
                      y = RballHull$arcs[,"c2"], 
                      start = (atan2(RballHull$arcs[,"v.x"], RballHull$arcs[,"v.y"])-RballHull$arcs[,"theta"]), 
                      end = (atan2(RballHull$arcs[,"v.x"], RballHull$arcs[,"v.y"])+RballHull$arcs[,"theta"]),
                      r = RballHull$arcs[,"r"])
figbplot <- ggplot() + 
            geom_arc(data=rbharcs, aes(x0=x, y0=y, r=r, start=start, end=end), color="blue", lineend="round") +
            geom_point(data=Rballs, aes(x=px,y=pz), alpha=0.3, color="blue", size=3, stroke=1) +
            geom_point(data=Rstrikes, aes(x=px,y=pz), alpha=0.3, color="red3", size=3, stroke=1, shape=23, fill="red3") +
            geom_polygon(data=rshpoints, aes(x=x, y=y), color="red", fill=NA, linetype="solid") +
            coord_fixed(xlim=c(-1.7,1.7), ylim=c(0.8,4.2)) + 
            theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
            ggtitle(prettyGID(gameID), subtitle="vs. right-handed batters")
LballHull <- ahull(Lballs, alpha=alph)
lbharcs <- data.frame(x = LballHull$arcs[,"c1"],
                      y = LballHull$arcs[,"c2"], 
                      start = (atan2(LballHull$arcs[,"v.x"], LballHull$arcs[,"v.y"])-LballHull$arcs[,"theta"]), 
                      end = (atan2(LballHull$arcs[,"v.x"], LballHull$arcs[,"v.y"])+LballHull$arcs[,"theta"]),
                      r = LballHull$arcs[,"r"])
LstrikeHull <- ahull(Lstrikes, alpha=10000) 
lshpoints <- data.frame(x = LstrikeHull$xahull[LstrikeHull$arcs[,"end1"],1], 
                        y = LstrikeHull$xahull[LstrikeHull$arcs[,"end1"],2])
ebzlplot <- ggplot() + 
            geom_arc(data=lbharcs, aes(x0=x, y0=y, r=r, start=start, end=end), color="blue", lineend="round") +
            geom_point(data=Lballs, aes(x=px,y=pz), alpha=0.3, color="blue", size=3, stroke=1) +
            geom_point(data=Lstrikes, aes(x=px,y=pz), alpha=0.3, color="red3", size=3, stroke=1, shape=23, fill="red3") +
            geom_polygon(data=lshpoints, aes(x=x, y=y), color="red", fill=NA, linetype="solid") +
            coord_fixed(xlim=c(-1.7,1.7), ylim=c(0.8,4.2)) + 
            theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
            ggtitle(prettyGID(gameID), subtitle="vs. left-handed batters")
rrects <- data.frame(xmin=ilr$M_rhh[,"xmin_r"], ymin=ilr$M_rhh[,"ymin_r"], 
                     xmax=ilr$M_rhh[,"xmax_r"], ymax=ilr$M_rhh[,"ymax_r"])
srrplot <- ggplot() + 
           geom_point(data=Rballs, aes(x=px,y=pz), alpha=0.3, color="blue", size=3, stroke=1) +
           geom_point(data=Rstrikes, aes(x=px,y=pz), alpha=0.3, color="red3", size=3, stroke=1, shape=23, fill="red3") +
           geom_rect(data=rrects, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), 
                     color="red", fill=NA, linetype="solid", size=0.3) +
           coord_fixed(xlim=c(-1.7,1.7), ylim=c(0.8,4.2)) + 
           theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
           ggtitle(prettyGID(gameID), subtitle="vs. right-handed batters")
ggsave("figures/response_examplesB.pdf", plot = grid.arrange(figbplot, srrplot, ncol=2), 
       width = 7, height = 4, dpi = 300)