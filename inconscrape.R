library("pitchRx")
inconsistency <- function(A,B) {
  pairs <- expand.grid(A,B)
  diffs <- pairs[,1]-pairs[,2]
  Iab <- 1/(length(A)*length(B))*sum(diffs[diffs>0])
  return(Iab/(mean(B)-mean(A)))
}
gameIDs <- "gid_2017_08_12_chnmlb_arimlb_1"

makeInconsistencyDF <- function(gameIDs) 
{
  n <- length(gameIDs)
  outDF <- data.frame(ump = character(n), 
                      gid = gameIDs,
                      lhOut = numeric(n), 
                      lhIn = numeric(n), 
                      rhOut = numeric(n), 
                      rhIn = numeric(n),
                      up = numeric(n),
                      down = numeric(n),
                      stringsAsFactors = FALSE)
  # TODO continue here
  
  for (i in 1:n) {
    playerdata <- scrape(game.ids=gameIDs[i], suffix="players.xml")
    outDF <- playerdata$umpire[playerdata$umpire$position=="home", "name"]
    gamedata <- scrape(game.ids=gameID)
    pitchdata <- gamedata$pitch # all pitches
    
  }
  
  
}
for(i in gameIDs) {
  playerdata <- scrape(game.ids=gameID, suffix="players.xml")
  umpName <- playerdata$umpire[playerdata$umpire$position=="home", "name"]
  gamedata <- scrape(game.ids=gameID)
  pitchdata <- gamedata$pitch # all pitches
  
}

playerdata <- scrape(game.ids=gameID, suffix="players.xml")
umpName <- playerdata$umpire[playerdata$umpire$position=="home", "name"]
gamedata <- scrape(game.ids=gameID)
cat("Inconsistency index for ", umpName, ". GID: ", gameID, "\n", sep="")
pitchdata <- gamedata$pitch # all pitches
epsilon <- 0.25 # how much of the corners to ignore
# pitchdata <- gamedata$pitch[(gamedata$pitch$px>0)&
#                               (gamedata$pitch$pz<gamedata$pitch$sz_top-epsilon)&
#                               (gamedata$pitch$pz>gamedata$pitch$sz_bot+epsilon),] # middle up/down, center to outside
atbatdata <- gamedata$atbat
numL <- atbatdata[atbatdata$stand=="L","num"]
numR <- atbatdata[atbatdata$stand=="R","num"]
Lpitchdata <- pitchdata[pitchdata$num %in% numL,]
Rpitchdata <- pitchdata[pitchdata$num %in% numR,]

#check calls vs. LH batters
umpZoneTop <- quantile(Lpitchdata[Lpitchdata$des=="Called Strike","pz"],0.95)
umpZoneBot <- quantile(Lpitchdata[Lpitchdata$des=="Called Strike","pz"],0.05)
pitchSample <- Lpitchdata[which((Lpitchdata$px>0)
                               &(Lpitchdata$pz>umpZoneBot)
                               &(Lpitchdata$pz<umpZoneTop)),]
Lballs <- pitchSample[pitchSample$des=="Ball",c("px","pz")]
LcalledStrikes <- pitchSample[pitchSample$des=="Called Strike",c("px","pz")]
plot(Lballs,xlim=c(-4,4),ylim=c(0,6))
points(LcalledStrikes,col="red",pch=5)
ballsX <- pitchSample[pitchSample$des=="Ball", "px"]
strikesX <- pitchSample[pitchSample$des=="Called Strike", "px"]
cat("LH outside inconsistency = ", inconsistency(strikesX, ballsX), "\n")

pitchSample <- Lpitchdata[which((Lpitchdata$px<0)
                               &(Lpitchdata$pz>umpZoneBot)
                               &(Lpitchdata$pz<umpZoneTop)),]
Lballs <- pitchSample[pitchSample$des=="Ball",c("px","pz")]
LcalledStrikes <- pitchSample[pitchSample$des=="Called Strike",c("px","pz")]
plot(Lballs,xlim=c(-4,4),ylim=c(0,6))
points(LcalledStrikes,col="red",pch=5)
ballsX <- pitchSample[pitchSample$des=="Ball", "px"]
strikesX <- pitchSample[pitchSample$des=="Called Strike", "px"]
cat("LH inside inconsistency = ", inconsistency(ballsX, strikesX), "\n")

#check calls vs. RH batters
umpZoneTop <- quantile(Rpitchdata[Rpitchdata$des=="Called Strike","pz"],0.95)
umpZoneBot <- quantile(Rpitchdata[Rpitchdata$des=="Called Strike","pz"],0.05)
pitchSample <- Rpitchdata[which((Rpitchdata$px>0)
                               &(Rpitchdata$pz>umpZoneBot)
                               &(Rpitchdata$pz<umpZoneTop)),]
Rballs <- pitchSample[pitchSample$des=="Ball",c("px","pz")]
RcalledStrikes <- pitchSample[pitchSample$des=="Called Strike",c("px","pz")]
plot(Rballs,xlim=c(-4,4),ylim=c(0,6))
points(RcalledStrikes,col="red",pch=5)
ballsX <- pitchSample[pitchSample$des=="Ball", "px"]
strikesX <- pitchSample[pitchSample$des=="Called Strike", "px"]
cat("RH outside inconsistency = ", inconsistency(strikesX, ballsX), "\n")

pitchSample <- Rpitchdata[which((Rpitchdata$px<0)
                               &(Rpitchdata$pz>umpZoneBot)
                               &(Rpitchdata$pz<umpZoneTop)),]
Rballs <- pitchSample[pitchSample$des=="Ball",c("px","pz")]
RcalledStrikes <- pitchSample[pitchSample$des=="Called Strike",c("px","pz")]
plot(Rballs,xlim=c(-4,4),ylim=c(0,6))
points(RcalledStrikes,col="red",pch=5)
ballsX <- pitchSample[pitchSample$des=="Ball", "px"]
strikesX <- pitchSample[pitchSample$des=="Called Strike", "px"]
cat("RH inside inconsistency = ", inconsistency(ballsX, strikesX), "\n")

