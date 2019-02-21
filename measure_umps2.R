# This script will produce a data frame containing several consistency and accuracy measurements
# on each MLB umpire.
# Computes everything in the original version, plus average gradient

dgrad1 <- function(d, x, y) {
  ix <- which.min(abs(d$x - x))
  iy <- which.min(abs(d$y - y))
  R <- d$z[ix + 1, iy]
  L <- d$z[ix-1, iy]
  deltax <- d$x[2] - d$x[1]
  deltay <- d$y[2] - d$y[1]
  dx <- ((R - L) / (2*deltax))
  R <- d$z[ix, iy + 1]
  L <- d$z[ix, iy-1]
  dy <- ((R - L) / (2*deltay))
  return(c(dx, dy))
}
vgrad<- Vectorize(dgrad1, vectorize.args = c("x", "y"))

library(dplyr)
library(tibble)
library(sp)
library(rgeos)
library(pracma)
library(MASS)
pitches <- as_data_frame(readRDS("pitches2017.Rda"))
games17inc <- as_data_frame(readRDS("games17inc.Rda"))
games17inc <- subset(games17inc, npitch>=50) # throw out games with less than 50 pitches
games17inc <- subset(games17inc, npitch<=300) # throw out games with more than 300 pitches

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
accS <- numeric(numumps) # season accuracy using 2017 umpire's zone (self-accuracy)
errHD <- numeric(numumps) # Hausdorff distance from consensus KDE contours
errSD <- numeric(numumps) # Area of symmetric difference with consensus KDE contours
czMAD <- numeric(numumps) # mean absolute difference of sample of CDF estimates
zsize <- numeric(numumps) # season zone size (area of KDE contour)
rBB <- numeric(numumps) # walk rate
rK <- numeric(numumps) # strikeout rate 
aveGrad <- numeric(numumps) # average gradient of season zone (Left + Right)

# Rule book zone: up/down pz's have been normalized to go from
# 1.5 to 3.5. Width of baseball is 0.245 feet, so we add 1/2 of
# a baseball's width to each edge. Width of plate is 17 inches.
# (17/12)/2+0.245/2 = 0.8308333
rbzoneX <- c(-0.8308333, 0.8308333, 0.8308333, -0.8308333)
rbzoneY <- c(1.3775, 1.3775, 3.6225, 3.6225)

# Consensus zone: Pitches that are called strikes
# 50% or more of the time. Computed in consensus_zones.R.
czonepoly <- readRDS("conzonepoly.Rda")
conczKDE <- readRDS("conczKDE.Rda")

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
bll <- list(L=data_frame(), R=data_frame())
cp <- list(L=data_frame(), R=data_frame())
stkKDE <- list(L=list(), R=list())
cpKDE <- list(L=list(), R=list())
czKDE <- list(L=list(), R=list())
szcontour <- list(L=list(), R=list())
szcontourdf <- list(L=data.frame(), R=data.frame())
avgr <- list(L=list(), R=list())
for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  bll[[s]] <- balls[balls$stand==s,c("px","pz")]
  cp[[s]] <- calledPitches[calledPitches$stand==s,c("px","pz")]
  stkKDE[[s]] <- kde2d(stk[[s]]$px, stk[[s]]$pz, n=200, lims = c(-2,2,0,5))
  cpKDE[[s]] <- kde2d(cp[[s]]$px, cp[[s]]$pz, n=200, lims = c(-2,2,0,5))
  czKDE[[s]] <- stkKDE[[s]]
  czKDE[[s]]$z <- czKDE[[s]]$z/cpKDE[[s]]$z*nrow(stk[[s]])/nrow(cp[[s]])

  szcontour[[s]] <- contourLines(czKDE[[s]], levels=0.5)
  szcontourdf[[s]] <- data.frame(px = szcontour[[s]][[1]]$x, pz = szcontour[[s]][[1]]$y)
  ## average gradient
  m<- vgrad(czKDE[[s]], szcontour[[s]][[1]]$x, szcontour[[s]][[1]]$y)
  mean(sqrt(m[1, ]^2 + m[2, ]^2))
  avgr[[s]]<- mean(sqrt(m[1, ]^2 + m[2, ]^2))
}
  aveGrad[i] <- avgr$L + avgr$R
  accS[i] <- (sum(point.in.polygon(subset(strikes, stand == "L")$px,
                                    subset(strikes, stand == "L")$pz,
                                    szcontourdf$L$px, szcontourdf$L$pz)) +
               sum(point.in.polygon(subset(strikes, stand == "R")$px,
                                    subset(strikes, stand == "R")$pz,
                                    szcontourdf$R$px, szcontourdf$R$pz)) +
               nrow(balls) -
               sum(point.in.polygon(subset(balls, stand == "L")$px,
                                    subset(balls, stand == "L")$pz,
                                    szcontourdf$L$px, szcontourdf$L$pz)) -
               sum(point.in.polygon(subset(balls, stand == "R")$px,
                                    subset(balls, stand == "R")$pz,
                                    szcontourdf$R$px, szcontourdf$R$pz))) /
              npitch[i]
  cpL <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(czonepoly$L))),ID="cpL")))
  cpR <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(czonepoly$R))),ID="cpR")))
  upL <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(szcontourdf$L))),ID="upL")))
  upR <- SpatialPolygons(list(Polygons(list(Polygon(as.matrix(szcontourdf$R))),ID="upR")))

  # Using rgeos:
  zsize[i] <- (gArea(upL) + gArea(upR))/2
  errHD[i] <- gDistance(cpL,upL, hausdorff = TRUE) + gDistance(cpR, upR, hausdorff = TRUE)
  errSD[i] <- gArea(gSymdifference(cpL, upL)) + gArea(gSymdifference(cpR, upR))
  czMAD[i] <- mean(abs(czKDE$L$z-conczKDE$L$z)) + mean(abs(czKDE$R$z-conczKDE$R$z))
  
  playdata <- pitchdata[!duplicated(pitchdata$play_guid.1),]
  num_walks <- sum(playdata$event == "Walk")
  rBB[i] <- num_walks/nrow(playdata)
  num_ks <- sum(playdata$event == "Strikeout")
  rK[i] <- num_ks/nrow(playdata)
  
  if((i %% 10) == 0) cat(".")
}

umps17 <- data.frame(umpid, umpname, ngames, npitch, aiR1, aiR10, aiIDX7, aiCH, aiACH7, 
                     accRB, accCZ, accS, errHD, errSD, czMAD, zsize, rBB, rK, aveGrad)
#saveRDS(umps17, file="umps17_2.Rda")
regularUmps17 <- umps17[umps17$ngames >= 20,]

