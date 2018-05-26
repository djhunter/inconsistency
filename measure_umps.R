# This script will produce a data frame containing several consistency and accuracy measurements
# on each MLB umpire.

library(dplyr)
library(tibble)
library(sp)
library(rgeos)
library(pracma)
library(ks)
# pitches <- as_data_frame(readRDS("pitches2017.Rda"))
games17inc <- as_data_frame(readRDS("games17inc.Rda"))
games17inc <- subset(games17inc, npitch>=50) # throw out games with less than 50 pitches

umpid <- unique(pitches$umpID)
numumps <- length(umpid)
ngames <- numeric(numumps)
npitch <- numeric(numumps)
umpname <- character(numumps)

# Average inconsistency metrics
aiR1 <- numeric(numumps)
aiR10 <- numeric(numumps)
aiIDX7 <- numeric(numumps)
aiCH <- numeric(numumps)
aiACH7 <- numeric(numumps)

accRB <- numeric(numumps) # season accuracy using rule-book rectangle
accCZ <- numeric(numumps) # season accuracy using 2017 consensus zone
errHD <- numeric(numumps) # Hausdorff distance from consensus KDE contours
errSD <- numeric(numumps) # Area of symmetric difference with consensus KDE contours
zsize <- numeric(numumps) # season zone size (area of KDE contour)
rBB <- numeric(numumps) # walk rate
rK <- numeric(numumps) # strikeout rate 

# Rule book zone: up/down pz's have been normalized to go from
# 1.5 to 3.5. Width of baseball is 0.245 feet, so we add 1/2 of
# a baseball's width to each edge. Width of plate is 17 inches.
# (17/12)/2+0.245/2 = 0.8308333
rbzoneX <- c(-0.8308333, 0.8308333, 0.8308333, -0.8308333)
rbzoneY <- c(1.3775, 1.3775, 3.6225, 3.6225)

# Consensus zones: convex hull of points that are called strikes
# 50% or more of the time. Computed in con_zones_roeg_LR.R.
czonepoly <- readRDS("conzonepoly50.Rda")
upper90kde <- readRDS("upper90kde17.Rda")

for(i in 1:numumps) {
#for(i in 1:2) { # for testing
  uid <- umpid[i]
  pitchdata <- subset(pitches, umpID == uid)
  calledPitches <- pitchdata[pitchdata$des=="Ball" | 
                             pitchdata$des=="Ball In Dirt" | 
                             pitchdata$des=="Called Strike", c("px","pz","des","stand")]
  calledPitches <- calledPitches[!is.na(calledPitches[,1]),]
  npitch[i] <- nrow(calledPitches)
  ngames[i] <- length(unique(pitchdata$gameday_link))
  umpname[i] <- as.character(pitchdata$umpName[1])
  incMetrics <- subset(games17inc, umpid == uid)
  aiR1[i] <- mean(incMetrics$incR1)
  aiR10[i] <- mean(incMetrics$incR10)
  aiIDX7[i] <- mean(incMetrics$incIDX7)
  aiCH[i] <- mean(incMetrics$incCH)
  aiACH7[i] <- mean(incMetrics$incACH7)
  
  balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                         c("px", "pz", "stand")]
  strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]
  
  accRB[i] <- (sum(point.in.polygon(strikes$px, strikes$pz, rbzoneX, rbzoneY)) +
              nrow(balls) - sum(point.in.polygon(balls$px, balls$pz, rbzoneX, rbzoneY))) /
              npitch[i]
  accCZ[i] <- (sum(point.in.polygon(subset(strikes, stand == "L")$px,
                                    subset(strikes, stand == "L")$pz,
                                    czonepoly$L$px, czonepoly$L$pz)) +
               sum(point.in.polygon(subset(strikes, stand == "R")$px,
                                    subset(strikes, stand == "R")$pz,
                                    czonepoly$R$px, czonepoly$R$pz)) +
               nrow(balls) -
               sum(point.in.polygon(subset(balls, stand == "L")$px,
                                    subset(balls, stand == "L")$pz,
                                    czonepoly$L$px, czonepoly$L$pz)) -
               sum(point.in.polygon(subset(balls, stand == "R")$px,
                                    subset(balls, stand == "R")$pz,
                                    czonepoly$R$px, czonepoly$R$pz))) /
              npitch[i]
  
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
  
  cpL <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(upper90kde$L))),ID="upL")))
  cpR <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(upper90kde$R))),ID="upR")))
  upL <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(szcontourdf$L))),ID="cpL")))
  upR <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(szcontourdf$R))),ID="cpR")))

  # Using rgeos:
  zsize[i] <- (gArea(upL) + gArea(upR))/2
  errHD[i] <- gDistance(cpL,upL, hausdorff = TRUE) + gDistance(cpR, upR, hausdorff = TRUE)
  errSD[i] <- gArea(gSymdifference(cpL, upL)) + gArea(gSymdifference(cpR, upR))
    
  # Using pracma polyarea: 
  # zsize[i] <- (abs(with(szcontour$L,polyarea(x,y))) + abs(with(szcontour$R,polyarea(x,y))))/2
  # Using pracma hausdorff_dist: 
  #errHD[i] <- hausdorff_dist(as.matrix(szcontourdf$L), as.matrix(upper90kde$L)) + 
  #            hausdorff_dist(as.matrix(szcontourdf$R), as.matrix(upper90kde$R))  
  if((i %% 10) == 0) cat(".")
}

umps17 <- data.frame(umpid, umpname, ngames, npitch, aiR1, aiR10, aiIDX7, aiCH, aiACH7, 
                     accRB, accCZ, errHD, errSD, zsize, rBB, rK)
saveRDS(umps17, file="umps17.Rda")
regularUmps17 <- umps17[umps17$ngames >= 20,]

