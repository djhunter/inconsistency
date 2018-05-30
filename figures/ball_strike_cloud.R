# plot of all 2017 balls and strikes
library(dplyr)
library(tibble)
library(ggplot2)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

# Rule book zone: up/down pz's have been normalized to go from
# 1.5 to 3.5. Width of baseball is 0.245 feet, so we add 1/2 of
# a baseball's width to each edge. Width of plate is 17 inches.
# (17/12)/2+0.245/2 = 0.8308333
rbzoneX <- c(-0.8308333, 0.8308333, 0.8308333, -0.8308333)
rbzoneY <- c(1.3775, 1.3775, 3.6225, 3.6225)
rbpoly <- data.frame(x=rbzoneX, y=rbzoneY)

calledPitches <- pitches[pitches$des=="Ball" | 
                           pitches$des=="Ball In Dirt" | 
                           pitches$des=="Called Strike", c("px","pz","des","stand")]
calledPitches <- calledPitches[!is.na(calledPitches[,1]),]
npitch <- nrow(calledPitches)
  
balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]
  
stk <- list(L=data_frame(), R=data_frame())
bll <- list(L=data_frame(), R=data_frame())
for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  bll[[s]] <- balls[balls$stand==s,c("px","pz")]
}
  
strikePlot <- list(L=list(), R=list())
ballPlot <- list(L=list(), R=list())
 
s <- "L"
strikePlot[[s]] <- ggplot() + 
            geom_point(data=stk[[s]], aes(x=px,y=pz), alpha=0.06, color="red", size=1, shape=20) +
            geom_polygon(data=rbpoly, aes(x=x, y=y), color="black", fill=NA, linetype="solid") +
            coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4.0)) + 
            theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
            ggtitle("Called Strikes, 2017", subtitle="vs. left-handed batters")
ballPlot[[s]] <- ggplot() + 
            geom_point(data=bll[[s]], aes(x=px,y=pz), alpha=0.06, color="blue", size=1, shape=20) +
            geom_polygon(data=rbpoly, aes(x=x, y=y), color="black", fill=NA, linetype="solid") +
            coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4.0)) + 
            theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
            ggtitle("Called Balls, 2017", subtitle="vs. left-handed batters")
s <- "R"
strikePlot[[s]] <- ggplot() + 
            geom_point(data=stk[[s]], aes(x=px,y=pz), alpha=0.06, color="red", size=1, shape=20) +
            geom_polygon(data=rbpoly, aes(x=x, y=y), color="black", fill=NA, linetype="solid") +
            coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4.0)) + 
            theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
            ggtitle(" ", subtitle="vs. right-handed batters")
ballPlot[[s]] <- ggplot() + 
            geom_point(data=bll[[s]], aes(x=px,y=pz), alpha=0.06, color="blue", size=1, shape=20) +
            geom_polygon(data=rbpoly, aes(x=x, y=y), color="black", fill=NA, linetype="solid") +
            coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4.0)) + 
            theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
            ggtitle(" ", subtitle="vs. right-handed batters")
require(gridExtra)
bscloud <- grid.arrange(strikePlot$L, strikePlot$R, ballPlot$L, ballPlot$R, ncol=4)
ggsave("figures/ball_strike_cloud.pdf", plot = bscloud, width = 10, height = 3.3, dpi = 300)

# to reduce size:
# pdf2ps ball_strike_cloud.pdf ball_strike_cloud.eps
# ps2pdf -dPDFSETTINGS=/printer ball_strike_cloud.eps ball_strike_cloud_printer.pdf