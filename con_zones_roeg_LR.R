# Compute consensus zones using Jon Roegele's method

library(sp)
library(ggplot2)
library(dplyr)
library(tibble)
require(grDevices)

#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

strikes <- pitches[pitches$des=="Called Strike",c("px","pz","stand","sz_top", "sz_bot")]
# normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
strikes$pz <- 2.0*(strikes$pz-strikes$sz_top)/(strikes$sz_top-strikes$sz_bot)+3.5
strikes <- strikes[!is.na(strikes[,1]),]

balls <- pitches[pitches$des=="Ball" | pitches$des=="Ball In Dirt",
                 c("px","pz","stand","sz_top", "sz_bot")]
# normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
balls$pz <- 2.0*(balls$pz-balls$sz_top)/(balls$sz_top-balls$sz_bot)+3.5
balls <- balls[!is.na(balls[,1]),]

nxy <- 1000 # number of grid squares in each direction
xmin <- -1.5
xmax <- 1.5
ymin <- 1
ymax <- 4
xseq <- seq(xmin, xmax, length.out = nxy+1)
yseq <- seq(ymin, ymax, length.out = nxy+1)
dx <- (xmax-xmin)/nxy
dy <- (ymax-ymin)/nxy
xcenters <- seq(xmin+dx/2, xmax-dx/2, length.out = nxy)
ycenters <- seq(ymin+dy/2, ymax-dy/2, length.out = nxy)
centers <- data.frame(px = rep(xcenters, nxy), pz = rep(ycenters, each=nxy))
p_min <- 0.5 # min proportion for border
stk_min <- 2 # min number of strikes for consideration

# initialize L/R variables
stk <- list(L=data_frame(), R=data_frame())
bll <- list(L=data_frame(), R=data_frame())
tot <- list(L=numeric(0), R=numeric(0))
sprop <- list(L=matrix(numeric(nxy*nxy), ncol=nxy), R=matrix(numeric(nxy*nxy), ncol=nxy))
onborder <- list(L=numeric(0), R=numeric(0))
czonepoly <- list(L=data_frame(), R=data_frame())
for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  bll[[s]] <- balls[balls$stand==s,c("px","pz")]
  for(i in 1:nxy) {
    for(j in 1:nxy) {
      strikesInSquare <- sum(point.in.polygon(stk[[s]]$px, stk[[s]]$pz, 
                                              c(xseq[i], xseq[i+1], xseq[i+1], xseq[i], xseq[i]), 
                                              c(yseq[j], yseq[j], yseq[j+1], yseq[j+1], yseq[j])))
      ballsInSquare <- sum(point.in.polygon(bll[[s]]$px, bll[[s]]$pz, 
                                              c(xseq[i], xseq[i+1], xseq[i+1], xseq[i], xseq[i]), 
                                              c(yseq[j], yseq[j], yseq[j+1], yseq[j+1], yseq[j])))
      if (strikesInSquare >= stk_min) {
        sprop[[s]][i,j] <- strikesInSquare/(strikesInSquare+ballsInSquare)
      }
    }
  }
  onborder[[s]] <- which(sprop[[s]] >= p_min)
  czonepoly[[s]] <- centers[onborder[[s]],][chull(centers[onborder[[s]],]),]
}

strikePlot <- list(L=list(), R=list())
ballPlot <- list(L=list(), R=list())
 
for(s in c("L", "R")) {
  strikePlot[[s]] <- ggplot() + geom_point(data=stk[[s]], aes(x=px,y=pz), alpha=0.01, color="red3", size=3, stroke=1) +
                     geom_polygon(data=czonepoly[[s]], aes(x=px, y=pz), color="black", fill=NA, linetype="solid") +
                     coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
                     theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  ballPlot[[s]] <- ggplot() + geom_point(data=bll[[s]], aes(x=px,y=pz), alpha=0.01, color="blue", size=3, stroke=1) +
                   geom_polygon(data=czonepoly[[s]], aes(x=px, y=pz), color="black", fill=NA, linetype="solid") +
                   coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
                   theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
}
require(gridExtra)
conzones <- grid.arrange(strikePlot$L, strikePlot$R, ballPlot$L, ballPlot$R, ncol=2)
ggsave("figures/con_zones_roeg.pdf", plot = conzones, width = 8, height = 8, dpi = 300)