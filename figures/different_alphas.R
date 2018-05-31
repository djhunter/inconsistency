# Figure ??: Comparing different alphas
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

# parameters for alpha-convex hull method
alph <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
n <- length(alph)

#gameID <- "gid_2017_08_12_chnmlb_arimlb_1"
#gameID <- "gid_2017_04_14_pitmlb_chnmlb_1" # balanced on each side
#gid <- "gid_2017_08_12_chnmlb_arimlb_1"
#gameID <- "gid_2017_08_20_miamlb_nynmlb_1"
#gid <- "gid_2017_08_12_chnmlb_arimlb_1" # not great game
#gameID <- "gid_2017_07_01_bosmlb_tormlb_1" # well-called game
#gameID <- "gid_2017_07_04_anamlb_minmlb_1"
#gameID <- "gid_2017_08_10_kcamlb_slnmlb_1" # good alpha-convex hull illustration
#gid <- "gid_2017_04_20_wasmlb_atlmlb_1" # only 30 calls
#gameID <- "gid_2017_06_04_chamlb_detmlb_1" # worst game by metric
#gid <- "gid_2017_05_12_cinmlb_sfnmlb_1"
#gid <- "gid_2017_08_27_detmlb_chamlb_1" # less than 3 strikes
gameID <- "gid_2017_09_15_slnmlb_chnmlb_1"
#gameID <- "gid_2017_09_16_slnmlb_chnmlb_1"
#gameID <- "gid_2017_08_13_colmlb_miamlb_1" # illustrates need for alpha-hull
#gameID <- "gid_2017_09_22_phimlb_atlmlb_1"

s <- "R" # side of plate to consider
aplots <- replicate(n, list())

for(i in 1:n) {
  pitchdata <- subset(pitches, gameday_link == gameID)
  pitchdata <- pitchdata[pitchdata$stand == s,]
  # normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
  pitchdata$pz <- 2.0*(pitchdata$pz-pitchdata$sz_top)/(pitchdata$sz_top-pitchdata$sz_bot)+3.5
  balls <- pitchdata[pitchdata$des=="Ball" | pitchdata$des=="Ball In Dirt",c("px","pz")]
  strikes <- pitchdata[pitchdata$des=="Called Strike",c("px","pz")]
  balls <- balls[!is.na(balls[,1]),]
  strikes <- strikes[!is.na(strikes[,1]),]
  ballHull <- ahull(balls, alpha=alph[i])
  strikeHull <- ahull(strikes, alpha=10000) 
  shpoints <- data.frame(x = strikeHull$xahull[strikeHull$arcs[,"end1"],1], 
                          y = strikeHull$xahull[strikeHull$arcs[,"end1"],2])
  bharcs <- data.frame(x = ballHull$arcs[,"c1"],
                        y = ballHull$arcs[,"c2"], 
                        start = (atan2(ballHull$arcs[,"v.x"], ballHull$arcs[,"v.y"])-ballHull$arcs[,"theta"]), 
                        end = (atan2(ballHull$arcs[,"v.x"], ballHull$arcs[,"v.y"])+ballHull$arcs[,"theta"]),
                        r = ballHull$arcs[,"r"])
  aplots[[i]] <- ggplot() + 
              geom_arc(data=bharcs, aes(x0=x, y0=y, r=r, start=start, end=end), color="blue", lineend="round") +
              geom_point(data=balls, aes(x=px,y=pz), alpha=0.3, color="blue", size=3, stroke=1) +
              geom_point(data=strikes, aes(x=px,y=pz), alpha=0.3, color="red3", size=3, stroke=1, shape=23, fill="red3") +
              geom_polygon(data=shpoints, aes(x=x, y=y), color="red", fill=NA, linetype="solid") +
              coord_fixed(xlim=c(-1.7,1.7), ylim=c(0.8,4.2)) + 
              theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
              ggtitle(bquote(alpha == .(alph[i])))
}

ggsave("figures/different_alphas.pdf", plot = do.call("grid.arrange", c(aplots, ncol=n)), 
       width = 16, height = 3, dpi = 300)
