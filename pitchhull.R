library(pitchRx)
library(alphahull)
library(geometry)

#gameID <- "gid_2017_08_12_chnmlb_arimlb_1"
#gameID <- "gid_2017_08_20_miamlb_nynmlb_1"
# gameID <- "gid_2017_08_12_chnmlb_arimlb_1" # not great game
gameID <- "gid_2017_07_01_bosmlb_tormlb_1" # well-called game
#gameID <- "gid_2017_07_04_anamlb_minmlb_1"
#gameID <- "gid_2017_07_05_nynmlb_wasmlb_1" #broken
#gameID <- "gid_2017_08_10_kcamlb_slnmlb_1"
playerdata <- scrape(game.ids=gameID, suffix="players.xml")
umpName <- playerdata$umpire[playerdata$umpire$position=="home", "name"]
umpID <- playerdata$umpire[playerdata$umpire$position=="home", "id"]

gamedata <- scrape(game.ids=gameID)
pitchdata <- gamedata$pitch # all pitches
# normalize up/down locations based on height of batter
pitchdata$pz <- 2.0*(pitchdata$pz-pitchdata$sz_top)/(pitchdata$sz_top-pitchdata$sz_bot)+3.5
atbatdata <- gamedata$atbat
numL <- atbatdata[atbatdata$stand=="L","num"]
numR <- atbatdata[atbatdata$stand=="R","num"]
Lpitchdata <- pitchdata[pitchdata$num %in% numL,]
Rpitchdata <- pitchdata[pitchdata$num %in% numR,]
Lballs <- Lpitchdata[Lpitchdata$des=="Ball",c("px","pz")]
Lstrikes <- Lpitchdata[Lpitchdata$des=="Called Strike",c("px","pz")]
Rballs <- Rpitchdata[Rpitchdata$des=="Ball",c("px","pz")]
Rstrikes <- Rpitchdata[Rpitchdata$des=="Called Strike",c("px","pz")]
Lballs <- Lballs[!is.na(Lballs[,1]),]
Rballs <- Rballs[!is.na(Rballs[,1]),]
Lstrikes <- Lstrikes[!is.na(Lstrikes[,1]),]
Rstrikes <- Rstrikes[!is.na(Rstrikes[,1]),]

plot(Rballs,xlim=c(-4,4),ylim=c(0,6), asp=1)
title(main=paste(c(umpName, gameID, " vs. Right-handed batters")))
points(Rstrikes,col="red",pch=5)
RstrikeHull <- ahull(Rstrikes, alpha=10000) # equals convex hull (approx)
plot(RstrikeHull, add=TRUE, wpoints=FALSE, col=c(2,0,0,0,0,0))
###
# now search for biggest alpha so that center of zone is not in alpha-hull
# alphaR = 10 # too big
# alphaL = 0.01 # too small
# middle = c(0,2) # center of strike zone
# epsilon = 0.01
# while(alphaR - alphaL > epsilon) {
#   alpha <- (alphaR+alphaL)/2
#   RballHull <- ahull(na.omit(Rballs), alpha=alpha)
#   if(inahull(RballHull, middle))
#     alphaR = alpha
#   else
#     alphaL = alpha
# }
# plot(RballHull, add=TRUE, wpoints=FALSE, col=c(3,0,0,0,0,0))
# cat("alpha for right-handed batters: ", alpha)
### alternative to searching:
alpha = 0.6 # could pass as parameter: bigger seems less fair to umpires
RballHull <- ahull(Rballs, alpha=alpha)
plot(RballHull, add=TRUE, wpoints=FALSE, col=c(3,0,0,0,0,0))
###

plot(Lballs,xlim=c(-4,4),ylim=c(0,6))
title(main=paste(c(umpName, gameID, " vs. Left-handed batters")))
points(Lstrikes,col="red",pch=5)
LstrikeHull <- ahull(Lstrikes, alpha=10000) # equals convex hull (approx)
# lines(Lstrikes[c(LstrikeHull$arcs[,7],LstrikeHull$arcs[1,7]),],col=2) # plot convex hull
plot(LstrikeHull, add=TRUE, wpoints=FALSE, col=c(2,0,0,0,0,0))
### 
# now search for biggest alpha so that center of zone is not in alpha-hull
# alphaR = 10 # too big
# alphaL = 0.01 # too small
# middle = c(0,2) # center of strike zone
# epsilon = 0.01
# while(alphaR - alphaL > epsilon) {
#   alpha <- (alphaR+alphaL)/2
#   LballHull <- ahull(Lballs, alpha=alpha)
#   if(inahull(LballHull, middle))
#     alphaR = alpha
#   else
#     alphaL = alpha
# }
# plot(LballHull, add=TRUE, wpoints=FALSE, col=c(3,0,0,0,0,0))
# cat("alpha for left-handed batters: ", alpha)
### alternatively:
alpha = 0.6 # could pass as parameter: bigger seems less fair to umpires
LballHull <- ahull(Lballs, alpha=alpha)
plot(LballHull, add=TRUE, wpoints=FALSE, col=c(3,0,0,0,0,0))
###

incon <- (sum(inahull(LballHull, matrix(unlist(Lstrikes), ncol=2, byrow=FALSE)))
          +sum(inahull(LstrikeHull, matrix(unlist(Lballs), ncol=2, byrow=FALSE)))
          +sum(inahull(RballHull, matrix(unlist(Rstrikes), ncol=2, byrow=FALSE)))
          +sum(inahull(RstrikeHull, matrix(unlist(Rballs), ncol=2, byrow=FALSE))))/
          (nrow(Lstrikes)+nrow(Lballs)+nrow(Rstrikes)+nrow(Rballs))
cat("Total inconsistency: ", incon)
