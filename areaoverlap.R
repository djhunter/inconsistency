library(alphahull)
library(geometry)

alpha <-  0.55388 # 1/3 width of strike zone
#alpha <- 0.8
searchForAlpha <- FALSE
#searchForAlpha <- TRUE

makeHullDF <- function(apdf, alpha=0.55388) 
{
  gameIDs <- unique(apdf$gameday_link)
  n <- length(gameIDs)
  outDF <- data.frame(umpName = character(n),
                      umpID = numeric(n),
                      gid = gameIDs,
                      lhCSinBallHull = numeric(n), 
                      rhCSinBallHull = numeric(n), 
                      lhCBinStrikeHull = numeric(n), 
                      rhCBinStrikeHull = numeric(n), 
                      totalCalls = numeric(n), 
                      inconIdx = numeric(n),
                      overlapL = numeric(n),
                      overlapR = numeric(n),
                      overlap = numeric(n),
                      stringsAsFactors = FALSE)
  
  if (is.null(alpha)) {
    optimize_alpha = TRUE
    cat("Optimizing for largest alpha.\n")
  } 
  else {
    optimize_alpha = FALSE
    cat(paste0("Using alpha = ", alpha, ".\n"))
  }
  # set up samples for measuring overlap
  deltaxy <- 0.01
  pxsamp <- seq(-1.5, 1.5, by=deltaxy)
  pysamp <- seq(1,4,by=deltaxy)
  sampPts <- as.matrix(expand.grid(pxsamp, pysamp))

  for (i in 1:n) {
    if((i %% 10) == 0) cat(".")
    if((i %% 100) == 0) cat(" Processed", i, "games.\n")
    pitchdata <- subset(apdf, gameday_link == gameIDs[i])
    outDF[i,"umpName"] <- as.character(pitchdata[1,"umpName"])
    outDF[i,"umpID"] <- pitchdata[1,"umpID"]
    # normalize up/down locations based on height of batter
    pitchdata$pz <- 2.0*(pitchdata$pz-pitchdata$sz_top)/(pitchdata$sz_top-pitchdata$sz_bot)+3.5
    Lpitchdata <- subset(pitchdata, stand=="L")
    Rpitchdata <- subset(pitchdata, stand=="R")
    Lballs <- Lpitchdata[Lpitchdata$des=="Ball",c("px","pz")]
    Lstrikes <- Lpitchdata[Lpitchdata$des=="Called Strike",c("px","pz")]
    Rballs <- Rpitchdata[Rpitchdata$des=="Ball",c("px","pz")]
    Rstrikes <- Rpitchdata[Rpitchdata$des=="Called Strike",c("px","pz")]
    Lballs <- Lballs[!is.na(Lballs[,1]),]
    Rballs <- Rballs[!is.na(Rballs[,1]),]
    Lstrikes <- Lstrikes[!is.na(Lstrikes[,1]),]
    Rstrikes <- Rstrikes[!is.na(Rstrikes[,1]),]
    emptyhull <- ahull(c(-100, -101, -100), c(0,0,1), alpha=10000) # kludge: not really empty but no data will ever be in it
    if (nrow(Rstrikes)>2) RstrikeHull <- ahull(Rstrikes, alpha=10000) else RstrikeHull <- emptyhull
    if (nrow(Lstrikes)>2) LstrikeHull <- ahull(Lstrikes, alpha=10000) else LstrikeHull <- emptyhull
    if (nrow(Rballs)<=2)
      RballHull <- emptyhull
    else {
      if (optimize_alpha) {
        # now search for biggest alpha so that center of zone is not in alpha-hull
        alphaR = 10 # too big
        alphaL = 0.01 # too small
        middle = c(0,2) # center of strike zone
        epsilon = 0.01
        while(alphaR - alphaL > epsilon) {
          alpha <- (alphaR+alphaL)/2
          RballHull <- ahull(na.omit(Rballs), alpha=alpha)
          if(inahull(RballHull, middle))
            alphaR = alpha
          else
            alphaL = alpha
        }
        alpha = alphaL
      }
      RballHull <- ahull(Rballs, alpha=alpha)
    }
    
    if (nrow(Lballs)<=2)
      LballHull <- emptyhull
    else {
      if (optimize_alpha) {
        alphaR = 10 # too big
        alphaL = 0.01 # too small
        middle = c(0,2) # center of strike zone
        epsilon = 0.01
        while(alphaR - alphaL > epsilon) {
          alpha <- (alphaR+alphaL)/2
          LballHull <- ahull(Lballs, alpha=alpha)
          if(inahull(LballHull, middle))
            alphaR = alpha
          else
            alphaL = alpha
        }
        alpha = alphaL
      }
      LballHull <- ahull(Lballs, alpha=alpha)
    }
    totalCalls <- nrow(Lstrikes)+nrow(Lballs)+nrow(Rstrikes)+nrow(Rballs)
    badRballs <- sum(inahull(RstrikeHull, matrix(unlist(Rballs), ncol=2, byrow=FALSE)))
    badRstrikes <- sum(inahull(RballHull, matrix(unlist(Rstrikes), ncol=2, byrow=FALSE)))
    badLballs <- sum(inahull(LstrikeHull, matrix(unlist(Lballs), ncol=2, byrow=FALSE)))
    badLstrikes <- sum(inahull(LballHull, matrix(unlist(Lstrikes), ncol=2, byrow=FALSE)))
    outDF[i,"lhCSinBallHull"] <- badLstrikes
    outDF[i,"lhCBinStrikeHull"] <- badLballs
    outDF[i,"rhCSinBallHull"] <- badRstrikes
    outDF[i,"rhCBinStrikeHull"] <- badRballs
    outDF[i,"totalCalls"] <- totalCalls
    outDF[i,"inconIdx"] <- (badLballs+badLstrikes+badRballs+badRstrikes)/totalCalls
    
    outDF[i,"overlapL"] <- sum(inahull(LstrikeHull, sampPts) & inahull(LballHull, sampPts))/nrow(sampPts)*9
    outDF[i,"overlapR"] <- sum(inahull(RstrikeHull, sampPts) & inahull(RballHull, sampPts))/nrow(sampPts)*9
    outDF[i,"overlap"] <- outDF[i,"overlapL"] + outDF[i,"overlapR"]
  }
  cat("Finished. Processed", i, "games.\n")
  return(outDF)
}

#rs2017 <- readRDS("regSeason2017.Rda")
cat("Finished reading data from file.", "\n")

#umpDF <- makeHullDF(rs2017[1:917,]) # first three games, for testing
#umpDF <- makeHullDF(rs2017[1:2038,]) 
#umpDF <- makeHullDF(rs2017) 
umpDF <- readRDS("rs2017alpha55withoverlap.Rda")

umpDF <- umpDF[umpDF$totalCalls>50,] # eliminate small samples
uumpids <- unique(umpDF$umpID)
umpAveIncon <- data.frame(ID = uumpids, name=character(length(uumpids)), 
                          gamesCalled = numeric(length(uumpids)),
                          aveincon = numeric(length(uumpids)),
                          aveoverlap = numeric(length(uumpids)),
                          stringsAsFactors = FALSE)
for (i in 1:length(uumpids)) {
  umpAveIncon[i,"aveincon"] = mean(umpDF[umpDF$umpID==uumpids[i], "inconIdx" ], na.rm=TRUE)
  umpAveIncon[i,"aveoverlap"] = mean(umpDF[umpDF$umpID==uumpids[i], "overlap" ], na.rm=TRUE)
  umpAveIncon[i,"name"] = umpDF[umpDF$umpID==uumpids[i],"umpName"][1]
  umpAveIncon[i,"gamesCalled"] = sum(umpDF$umpID == uumpids[i])
}
# sortedumps <- umpAveIncon[order(umpAveIncon$aveincon),]
# print(sortedumps[,c("name","aveincon","aveoverlap")], row.names = FALSE, right=FALSE)

#throw out umps with fewer than 15 games
umpAveIncon15plus <- subset(umpAveIncon, gamesCalled>=15)

sortedumps <- umpAveIncon15plus[order(umpAveIncon15plus$aveincon),]
print(sortedumps[,c("name","aveincon","aveoverlap")], row.names = FALSE, right=FALSE)

sortedumps <- umpAveIncon15plus[order(umpAveIncon15plus$aveoverlap),]
print(sortedumps[,c("name","aveincon","aveoverlap")], row.names = FALSE, right=FALSE)
#saveRDS(umpDF, file="rs2017alpha55withoverlap.Rda")

ggplot(umpAveIncon15plus, aes(y=aveoverlap, x=aveincon, label=name))+
  geom_point() +geom_text(aes(label=name),hjust=-0.05, vjust=0, size=3)+
  labs(y="Average Overlap Area", x = "Average Inconsistency Index")

