# Determine the contour level to use so that average contour area equals aggregate
# 90% contour area (pardon some spaghetti code.)

library(dplyr)
library(tibble)
library(sp)
library(rgeos)
library(pracma)
library(ks)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

umpid <- unique(pitches$umpID)
numumps <- length(umpid)
ngames <- numeric(numumps)
npitch <- numeric(numumps)
umpname <- character(numumps)

zs90 <- numeric(numumps) # season zone size (area of KDE contour)
zs91 <- numeric(numumps) # season zone size (area of KDE contour)
zs92 <- numeric(numumps) # season zone size (area of KDE contour)
zs93 <- numeric(numumps) # season zone size (area of KDE contour)
zs94 <- numeric(numumps) # season zone size (area of KDE contour)
zs95 <- numeric(numumps) # season zone size (area of KDE contour)
zs96 <- numeric(numumps) # season zone size (area of KDE contour)
zs97 <- numeric(numumps) # season zone size (area of KDE contour)
zs98 <- numeric(numumps) # season zone size (area of KDE contour)
zs99 <- numeric(numumps) # season zone size (area of KDE contour)

# upper 90% contour of KDE of all strikes (L/R) for 2017
# computed in consensus_zones_LR.R
upper90kde <- readRDS("upper90kde17.Rda")

for(i in 1:numumps) {
# for(i in 1:2) { # for testing
  uid <- umpid[i]
  pitchdata <- subset(pitches, umpID == uid)
  calledPitches <- pitchdata[pitchdata$des=="Ball" | 
                             pitchdata$des=="Ball In Dirt" | 
                             pitchdata$des=="Called Strike", c("px","pz","des","stand")]
  calledPitches <- calledPitches[!is.na(calledPitches[,1]),]
  npitch[i] <- nrow(calledPitches)
  ngames[i] <- length(unique(pitchdata$gameday_link))
  umpname[i] <- as.character(pitchdata$umpName[1])
  
  balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                         c("px", "pz", "stand")]
  strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]
  
  stk <- list(L=data_frame(), R=data_frame())
  H_scv <- list(L = matrix(), R = matrix())
  fhat <- list(L=list(), R=list())
  szc90 <- list(L=list(), R=list())
  szc91 <- list(L=list(), R=list())
  szc92 <- list(L=list(), R=list())
  szc93 <- list(L=list(), R=list())
  szc94 <- list(L=list(), R=list())
  szc95 <- list(L=list(), R=list())
  szc96 <- list(L=list(), R=list())
  szc97 <- list(L=list(), R=list())
  szc98 <- list(L=list(), R=list())
  szc99 <- list(L=list(), R=list())
  for(s in c("L", "R")) {
    stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
    H_scv[[s]] <- Hscv(x=stk[[s]])
    fhat[[s]] <- kde(x=stk[[s]], H=H_scv[[s]], compute.cont=TRUE)
    szc90[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["10%"])[[1]])
    szc91[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["9%"])[[1]])
    szc92[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["8%"])[[1]])
    szc93[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["7%"])[[1]])
    szc94[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["6%"])[[1]])
    szc95[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["5%"])[[1]])
    szc96[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["4%"])[[1]])
    szc97[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["3%"])[[1]])
    szc98[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["2%"])[[1]])
    szc99[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate,levels=cont["1%"])[[1]])
  }
  
  upL90 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc90$L$x, szc90$L$y))),ID="upL")))
  upR90 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc90$R$x, szc90$R$y))),ID="upR")))
  zs90[i] <- (gArea(upL90) + gArea(upR90))/2
  upL91 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc91$L$x, szc91$L$y))),ID="upL")))
  upR91 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc91$R$x, szc91$R$y))),ID="upR")))
  zs91[i] <- (gArea(upL91) + gArea(upR91))/2
  upL92 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc92$L$x, szc92$L$y))),ID="upL")))
  upR92 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc92$R$x, szc92$R$y))),ID="upR")))
  zs92[i] <- (gArea(upL92) + gArea(upR92))/2
  upL93 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc93$L$x, szc93$L$y))),ID="upL")))
  upR93 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc93$R$x, szc93$R$y))),ID="upR")))
  zs93[i] <- (gArea(upL93) + gArea(upR93))/2
  upL94 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc94$L$x, szc94$L$y))),ID="upL")))
  upR94 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc94$R$x, szc94$R$y))),ID="upR")))
  zs94[i] <- (gArea(upL94) + gArea(upR94))/2
  upL95 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc95$L$x, szc95$L$y))),ID="upL")))
  upR95 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc95$R$x, szc95$R$y))),ID="upR")))
  zs95[i] <- (gArea(upL95) + gArea(upR95))/2
  upL96 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc96$L$x, szc96$L$y))),ID="upL")))
  upR96 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc96$R$x, szc96$R$y))),ID="upR")))
  zs96[i] <- (gArea(upL96) + gArea(upR96))/2
  upL97 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc97$L$x, szc97$L$y))),ID="upL")))
  upR97 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc97$R$x, szc97$R$y))),ID="upR")))
  zs97[i] <- (gArea(upL97) + gArea(upR97))/2
  upL98 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc98$L$x, szc98$L$y))),ID="upL")))
  upR98 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc98$R$x, szc98$R$y))),ID="upR")))
  zs98[i] <- (gArea(upL98) + gArea(upR98))/2
  upL99 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc99$L$x, szc99$L$y))),ID="upL")))
  upR99 <- SpatialPolygons(list(Polygons(list(Polygon(cbind(szc99$R$x, szc99$R$y))),ID="upR")))
  zs99[i] <- (gArea(upL99) + gArea(upR99))/2
  
  if((i %% 10) == 0) cat(".")
}

zsumps17 <- data.frame(umpid, umpname, ngames, npitch, 
                     zs90, zs91, zs92, zs93, zs94, zs95, zs96, zs97, zs98, zs99)  
regularZsUmps17 <- zsumps17[zsumps17$ngames >= 20,]

# We want the contour whose area averages to about 3.780195 (area of aggregate 90% contour)
