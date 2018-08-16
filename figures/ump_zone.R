# Compare ump zone to consensus zone
library(dplyr)
library(tibble)
library(MASS)
library(ggplot2)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

# Rule book zone: up/down pz's have been normalized to go from
# 1.5 to 3.5. Width of baseball is 0.245 feet, so we add 1/2 of
# a baseball's width to each edge. Width of plate is 17 inches.
# (17/12)/2+0.245/2 = 0.8308333
rbzoneX <- c(-0.8308333, 0.8308333, 0.8308333, -0.8308333)
rbzoneY <- c(1.3775, 1.3775, 3.6225, 3.6225)

# Consensus zone: Pitches that are called strikes
# 50% or more of the time. Computed in consensus_zones.R.
czonepoly <- readRDS("conzonepoly.Rda")

#uid <- 521889 # Stu Scheurwater (most conforming SD)
#uid <- 427128 # Rob Drake (least conforming SD)
#uid <- 427139 # Doug Eddings (largest)
#uid <- 427339 # Jerry Meals (smallest)
uid <- 431232 # Chad Fairchild (consistent ACH but not Rect)
uid <- 484183 # Cory Blaser (consistent but nonrectangular)
uid <- 489985 # Chad Whitson
uid <- 427197 # Chris Guccione
uid <- 573596 # Pat Hoberg

pitchdata <- subset(pitches, umpID == uid)
calledPitches <- pitchdata[pitchdata$des=="Ball" | 
                           pitchdata$des=="Ball In Dirt" | 
                           pitchdata$des=="Called Strike", c("px","pz","des","stand")]
calledPitches <- calledPitches[!is.na(calledPitches[,1]),]
npitch <- nrow(calledPitches)
ngames <- length(unique(pitchdata$gameday_link))
umpname <- as.character(pitchdata$umpName[1])
  
balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]
  
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
  
strikePlot <- list(L=list(), R=list())
 
s <- "L"
strikePlot[[s]] <- ggplot() + 
                     geom_path(data=czonepoly[[s]], aes(x=px, y=pz), color="gray") +
                     geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="red3") +
                     coord_fixed(xlim=c(-1.3,1.3), ylim=c(1.2,3.8)) + 
                     theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
                     ggtitle(umpname, subtitle="vs. left-handed batters")
s <- "R"
strikePlot[[s]] <- ggplot() + 
                     geom_path(data=czonepoly[[s]], aes(x=px, y=pz), color="gray") +
                     geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="red3") +
                     coord_fixed(xlim=c(-1.3,1.3), ylim=c(1.2,3.8)) + 
                     theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
                     ggtitle(" ", subtitle="vs. right-handed batters")
require(gridExtra)
umpzones <- grid.arrange(strikePlot$L, strikePlot$R, ncol=2)
