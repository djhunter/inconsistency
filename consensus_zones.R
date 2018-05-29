library(ggplot2)
library(MASS)
library(dplyr)
library(tibble)
pitches <- as_data_frame(readRDS("pitches2017.Rda"))

calledPitches <- pitches[pitches$des=="Ball" | 
                           pitches$des=="Ball In Dirt" | 
                           pitches$des=="Called Strike", c("px","pz","des","stand")]
calledPitches <- calledPitches[!is.na(calledPitches[,1]),]
npitch <- nrow(calledPitches)
  
balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]

# initialize L/R variables
stk <- list(L=data_frame(), R=data_frame())
bll <- list(L=data_frame(), R=data_frame())
cp <- list(L=data_frame(), R=data_frame())
stkKDE <- list(L=list(), R=list())
cpKDE <- list(L=list(), R=list())
czKDE <- list(L=list(), R=list())
szcontour <- list(L=list(), R=list())
szcontourdf <- list(L=data.frame(), R=data.frame())
for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  bll[[s]] <- balls[balls$stand==s,c("px","pz")]
  cp[[s]] <- calledPitches[calledPitches$stand==s,c("px","pz")]
  stkKDE[[s]] <- kde2d(stk[[s]]$px, stk[[s]]$pz, n=200, lims = c(-2,2,0,5))
  cpKDE[[s]] <- kde2d(cp[[s]]$px, cp[[s]]$pz, n=200, lims = c(-2,2,0,5))
  czKDE[[s]] <- stkKDE[[s]]
  czKDE[[s]]$z <- czKDE[[s]]$z/cpKDE[[s]]$z*nrow(stk$L)/nrow(cp$L)

  szcontour[[s]] <- contourLines(czKDE[[s]], levels=0.5)
  szcontourdf[[s]] <- data.frame(px = szcontour[[s]][[1]]$x, pz = szcontour[[s]][[1]]$y)
}
saveRDS(szcontourdf, "conzonepoly.Rda")

strikePlot <- list(L=list(), R=list())
ballPlot <- list(L=list(), R=list())
 
for(s in c("L", "R")) {
  strikePlot[[s]] <- ggplot() + # geom_point(data=stk[[s]], aes(x=px,y=pz), alpha=0.01, color="red3", size=3, stroke=1) +
                     geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="black") +
                     coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
                     theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  ballPlot[[s]] <- ggplot() + # geom_point(data=bll[[s]], aes(x=px,y=pz), alpha=0.01, color="blue", size=3, stroke=1) +
                   geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="black") +
                   coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
                   theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
}
require(gridExtra)
conzones <- grid.arrange(strikePlot$L, strikePlot$R, ncol=2)

#conzones <- grid.arrange(strikePlot$L, strikePlot$R, ballPlot$L, ballPlot$R, ncol=2)
#ggsave("figures/consensus_zones.pdf", plot = conzones, width = 8, height = 8, dpi = 300)

# to reduce size:
# pdf2ps consensus_zones.pdf consensus_zones.eps
# ps2pdf -dPDFSETTINGS=/printer consensus_zones.eps consensus_zones_printer.pdf
