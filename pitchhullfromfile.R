library(alphahull)
library(geometry)
library(scales)

#rs2017 <- readRDS("regSeason2017.Rda")

#alpha = 0.6 # could pass as parameter: bigger seems less fair to umpires
#alpha = 0.245 # diameter of baseball
#alpha = 0.7083333 # half of width of plate
#alpha = 1.41667 # width of plate
#alpha = 0.47222 # 1/3 width of plate
#alpha = 0.83083 # 1/2 width of strike zone
#alpha = 0.78929 # 95% of 1/2 width of strike zone
#alpha = 1.66166 # width of strike zone
#alpha <-  0.55388 # 1/3 width of strike zone
alpha <- 1.2
searchForAlpha <- FALSE
#searchForAlpha <- TRUE

#gameID <- "gid_2017_08_12_chnmlb_arimlb_1"
#gameID <- "gid_2017_08_20_miamlb_nynmlb_1"
#gameID <- "gid_2017_08_12_chnmlb_arimlb_1" # not great game
gameID <- "gid_2017_07_01_bosmlb_tormlb_1" # well-called game
#gameID <- "gid_2017_07_04_anamlb_minmlb_1"
#gameID <- "gid_2017_07_05_nynmlb_wasmlb_1"
#gameID <- "gid_2017_08_10_kcamlb_slnmlb_1"
#gameID <- "gid_2017_04_20_wasmlb_atlmlb_1" # only 30 calls
#gameID <- "gid_2017_06_04_chamlb_detmlb_1" # worst game by metric
#gameID <- "gid_2017_04_14_pitmlb_chnmlb_1" # balanced on each side
#gameID <- "gid_2017_05_12_cinmlb_sfnmlb_1"
#gameID <- "gid_2017_08_27_detmlb_chamlb_1" # less than 3 strikes
#gameID <- "gid_2017_09_15_slnmlb_chnmlb_1"
#gameID <- "gid_2017_09_16_slnmlb_chnmlb_1"
#gameID <- "gid_2017_10_12_chnmlb_wasmlb_1"
#gameID <- "gid_2017_10_29_lanmlb_houmlb_1" # world series game 5
#gameID <- "gid_2017_10_31_houmlb_lanmlb_1" # world series game 6

pitchdata <- subset(rs2017, gameday_link==gameID) # all pitches
# normalize up/down locations based on height of batter
pitchdata$normedpz <- 2.0*(pitchdata$pz-pitchdata$sz_top)/(pitchdata$sz_top-pitchdata$sz_bot)+3.5
Lballs <- subset(pitchdata, des=="Ball" & stand=="L",c("px","normedpz"))
Lstrikes <- subset(pitchdata, des=="Called Strike" & stand=="L",c("px","normedpz"))
Rballs <- subset(pitchdata, des=="Ball" & stand=="R",c("px","normedpz"))
Rstrikes <- subset(pitchdata, des=="Called Strike" & stand=="R",c("px","normedpz"))
Lballs <- Lballs[!is.na(Lballs[,1]),]
Rballs <- Rballs[!is.na(Rballs[,1]),]
Lstrikes <- Lstrikes[!is.na(Lstrikes[,1]),]
Rstrikes <- Rstrikes[!is.na(Rstrikes[,1]),]
umpName <- as.character(pitchdata[1,"umpName"])

#plot(Rballs,xlim=c(-4,4),ylim=c(0,6), asp=1)
ballsize=1.0
transp=0.4
x_range <- c(-2.5,2.5)
y_range <- c(0,5)
plot(Rballs,xlim=x_range, ylim=y_range, asp=1, col=alpha("black", transp), pch=19, cex=ballsize)
#plot(Rstrikes,xlim=c(-4,4),ylim=c(0,6), asp=1, col=alpha("red", transp), pch=19, cex=ballsize)
#title(main=paste(c(gameID, " vs. right-handed batters")))
#points(Rstrikes,col="red",pch=5)
points(Rstrikes, col=alpha("red", transp), pch=19, cex=ballsize)

emptyhull <- ahull(c(-100, -101, -100), c(0,0,1), alpha=100000) # kludge: not really empty but no data will ever be in it

if (nrow(Rstrikes)>2) RstrikeHull <- ahull(Rstrikes, alpha=10000) else RstrikeHull <- emptyhull
plot(RstrikeHull, add=TRUE, wpoints=FALSE, col=c(2,0,0,0,0,0))
###
if(searchForAlpha) {
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
  alpha <- alphaL
  cat("alpha for right-handed batters: ", alpha, "\n")
}
if (nrow(Rballs)>2) RballHull <- ahull(Rballs, alpha=alpha) else RballHull <- emptyhull
plot(RballHull, add=TRUE, wpoints=FALSE, col=c(4,0,0,0,0,0))
#title(main=paste0(c(umpName, ", ", gameID, "\nvs. right-handed batters, ", "alpha = ", alpha), collapse=""))
#title(main=paste0(c(gameID, "\nvs. RHB, ", "alpha = ", format(alpha, digits=4)), collapse=""))
title(main=paste0(c("alpha = ", format(alpha,digits=4)), collapse=""))

plot(Lballs,xlim=x_range,ylim=y_range, asp=1, col=alpha("black", transp), pch=19, cex=ballsize)
points(Lstrikes, col=alpha("red", transp), pch=19, cex=ballsize)
if (nrow(Lstrikes)>2) LstrikeHull <- ahull(Lstrikes, alpha=10000) else LstrikeHull <- emptyhull
plot(LstrikeHull, add=TRUE, wpoints=FALSE, col=c(2,0,0,0,0,0))
if(searchForAlpha) {
  #now search for biggest alpha so that center of zone is not in alpha-hull
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
  alpha <- alphaL
  cat("alpha for left-handed batters: ", alpha, "\n")
}
### alternatively:
if (nrow(Lballs)>2) LballHull <- ahull(Lballs, alpha=alpha) else LballHull <- emptyhull
plot(LballHull, add=TRUE, wpoints=FALSE, col=c(4,0,0,0,0,0))
#title(main=paste0(c(umpName, ", ", gameID, "\nvs. left-handed batters, ", "alpha = ", alpha), collapse=""))
#title(main=paste0(c(gameID, "\nvs. LHB, ", "alpha = ", format(alpha,digits=4)), collapse=""))
title(main=paste0(c("alpha = ", format(alpha,digits=4)), collapse=""))
###

incon <- (sum(inahull(LballHull, matrix(unlist(Lstrikes), ncol=2, byrow=FALSE)))
          +sum(inahull(LstrikeHull, matrix(unlist(Lballs), ncol=2, byrow=FALSE)))
          +sum(inahull(RballHull, matrix(unlist(Rstrikes), ncol=2, byrow=FALSE)))
          +sum(inahull(RstrikeHull, matrix(unlist(Rballs), ncol=2, byrow=FALSE))))/
          (nrow(Lstrikes)+nrow(Lballs)+nrow(Rstrikes)+nrow(Rballs))
cat("Total inconsistency: ", incon, "\n")
