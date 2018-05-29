# Compare ump KDE to consensus KDE
library(dplyr)
library(tibble)
library(sp)
library(rgeos)
library(pracma)
library(ks)
pitches <- as_data_frame(readRDS("pitches2017.Rda"))

# Rule book zone: up/down pz's have been normalized to go from
# 1.5 to 3.5. Width of baseball is 0.245 feet, so we add 1/2 of
# a baseball's width to each edge. Width of plate is 17 inches.
# (17/12)/2+0.245/2 = 0.8308333
rbzoneX <- c(-0.8308333, 0.8308333, 0.8308333, -0.8308333)
rbzoneY <- c(1.3775, 1.3775, 3.6225, 3.6225)

# Consensus zone: convex hull of points that are called strikes
# 50% or more of the time. Computed in con_zones_roeg_LR.R.
czonepoly <- readRDS("conzonepoly50.Rda")
# upper 90% contour of KDE of all strikes (L/R) for 2017
# computed in consensus_zones_LR.R
upper90kde <- readRDS("upper90kde17.Rda")

uid <- 484198 # Alan Porter (most conforming SD)
uid <- 427044 # CB Bucknor (least conforming SD)
uid <- 427139 # Doug Eddings (largest)
uid <- 427103 # Gerry Davis (smallest)

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
H_scv <- list(L = matrix(), R = matrix())
fhat <- list(L=list(), R=list())
szcontour <- list(L=list(), R=list())
szcontourdf <- list(L=data.frame(), R=data.frame())
for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  H_scv[[s]] <- Hscv(x=stk[[s]])
  fhat[[s]] <- kde(x=stk[[s]], H=H_scv[[s]], compute.cont=TRUE)
  szcontour[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                    z=estimate,levels=cont["10%"])[[1]])
  szcontourdf[[s]] <- data.frame(px = szcontour[[s]]$x, pz = szcontour[[s]]$y)
}
  
# TODO: make plots of these zones
