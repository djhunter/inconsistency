library(ggplot2)
library(dplyr)
library(tibble)
pitches <- as_data_frame(readRDS("pitches2017.Rda"))
szcontourdf <- readRDS("conzonepoly.Rda")
calledPitches <- pitches[pitches$des=="Ball" | 
                           pitches$des=="Ball In Dirt" | 
                           pitches$des=="Called Strike", c("px","pz","des","stand")]
calledPitches <- calledPitches[!is.na(calledPitches[,1]),]
npitch <- nrow(calledPitches)
  
balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]

xmin <- -1.5
xmax <- 1.5
ymin <- 1
ymax <- 4
dx <- dy <- 1/12 # 1 inch squares

xseq <- seq(xmin+dx/2, xmax, by = dx) # tile centers
yseq <- seq(ymin+dx/2, ymax, by = dy)


# initialize L/R variables
stk <- list(L=data_frame(), R=data_frame())
bll <- list(L=data_frame(), R=data_frame())
cp <- list(L=data_frame(), R=data_frame())
rzplot <- list(L=list(), R=list())
for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  bll[[s]] <- balls[balls$stand==s,c("px","pz")]
  cp[[s]] <- calledPitches[calledPitches$stand==s,c("px","pz")]
  gsInZone <- function(i,j) {
    sum(stk[[s]]$px < xseq[i]+dx/2 & stk[[s]]$px > xseq[i]- dx/2 & 
          stk[[s]]$pz < yseq[j] + dy/2 & stk[[s]]$pz > yseq[[j]]- dy/2) > 
    sum(bll[[s]]$px < xseq[i]+dx/2 & bll[[s]]$px > xseq[i]- dx/2 & 
          bll[[s]]$pz < yseq[j] + dy/2 & bll[[s]]$pz > yseq[[j]]- dy/2)  
  }
  g <- Vectorize(gsInZone)
  ijInZone <- which(outer(1:length(xseq), 1:length(yseq), FUN = g), arr.ind = TRUE)
  xInZone <- xseq[ijInZone[,1]]
  yInZone <- yseq[ijInZone[,2]]
  zdf <- data.frame(xInZone, yInZone)
  rzplot[[s]] <- geom_tile(data=zdf, aes(x=xInZone, y=yInZone), color="black", fill="blue", alpha=0.1)
}

strikePlot <- list(L=list(), R=list())
for(s in c("L", "R")) {
  strikePlot[[s]] <- ggplot() + 
                     rzplot[[s]] + 
                     geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="black") +
                     coord_fixed(xlim=c(-1.3,1.3), ylim=c(1.2,3.8)) + 
                     theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
                     ggtitle(ifelse(s=="L", "Consensus zones, 2017", " "),
                             subtitle=ifelse(s=="L", "vs. left-handed batters", "vs. right-handed batters"))
}
require(gridExtra)

conzones <- grid.arrange(strikePlot$L, strikePlot$R, ncol=2)
ggsave("figures/kdevsroeg.pdf", plot = conzones, width = 8, height = 4, dpi = 300)
