library("pitchRx")
inconsistency <- function(A,B) {
  if (length(A) == 0 | length(B) == 0)
    return(0)
  pairs <- expand.grid(A,B)
  diffs <- pairs[,1]-pairs[,2]
  Iab <- 1/(length(A)*length(B))*sum(diffs[diffs>0])
  return(Iab/(mean(B)-mean(A)))
}
# gameIDs <- "gid_2017_08_12_chnmlb_arimlb_1"

makeInconsistencyDF <- function(gameIDs) 
{
  n <- length(gameIDs)
  outDF <- data.frame(ump = character(n), 
                      gid = gameIDs,
                      lhOut = numeric(n), 
                      lhIn = numeric(n), 
                      rhOut = numeric(n), 
                      rhIn = numeric(n),
                      total = numeric(n),
                      stringsAsFactors = FALSE)
  # TODO continue here
  
  for (i in 1:n) {
    hitdata <- scrape(game.ids=gameIDs[i], suffix="inning/inning_hit.xml")
    if ( is.null(nrow(hitdata$hip))){
      outDF[i,"ump"] <- "<Rainout>"
      outDF[i,"total"] <- NA
    }
    else  { 
    playerdata <- scrape(game.ids=gameIDs[i], suffix="players.xml")
    outDF[i,"ump"] <- playerdata$umpire[playerdata$umpire$position=="home", "name"]
    gamedata <- scrape(game.ids=gameIDs[i])
    pitchdata <- gamedata$pitch # all pitches
    atbatdata <- gamedata$atbat
    numL <- atbatdata[atbatdata$stand=="L","num"]
    numR <- atbatdata[atbatdata$stand=="R","num"]
    Lpitchdata <- pitchdata[pitchdata$num %in% numL,]
    Rpitchdata <- pitchdata[pitchdata$num %in% numR,]
    #check calls vs. LH batters
    umpZoneTop <- quantile(Lpitchdata[Lpitchdata$des=="Called Strike","pz"],0.95,na.rm = TRUE)
    umpZoneBot <- quantile(Lpitchdata[Lpitchdata$des=="Called Strike","pz"],0.05,na.rm = TRUE)
    pitchSample <- Lpitchdata[which((Lpitchdata$px>0)
                               &(Lpitchdata$pz>umpZoneBot)
                               &(Lpitchdata$pz<umpZoneTop)),]
    ballsX <- pitchSample[pitchSample$des=="Ball", "px"]
    strikesX <- pitchSample[pitchSample$des=="Called Strike", "px"]
    outDF[i,"lhOut"] <- inconsistency(strikesX, ballsX)
    pitchSample <- Lpitchdata[which((Lpitchdata$px<0)
                               &(Lpitchdata$pz>umpZoneBot)
                               &(Lpitchdata$pz<umpZoneTop)),]
    ballsX <- pitchSample[pitchSample$des=="Ball", "px"]
    strikesX <- pitchSample[pitchSample$des=="Called Strike", "px"]
    outDF[i,"lhIn"] <- inconsistency(ballsX, strikesX)
    #check calls vs. RH batters
    umpZoneTop <- quantile(Rpitchdata[Rpitchdata$des=="Called Strike","pz"],0.95,na.rm = TRUE)
    umpZoneBot <- quantile(Rpitchdata[Rpitchdata$des=="Called Strike","pz"],0.05,na.rm = TRUE)
    pitchSample <- Rpitchdata[which((Rpitchdata$px>0)
                               &(Rpitchdata$pz>umpZoneBot)
                               &(Rpitchdata$pz<umpZoneTop)),]
    ballsX <- pitchSample[pitchSample$des=="Ball", "px"]
    strikesX <- pitchSample[pitchSample$des=="Called Strike", "px"]
    outDF[i,"rhOut"] <- inconsistency(strikesX, ballsX)
    pitchSample <- Rpitchdata[which((Rpitchdata$px<0)
                               &(Rpitchdata$pz>umpZoneBot)
                               &(Rpitchdata$pz<umpZoneTop)),]
    ballsX <- pitchSample[pitchSample$des=="Ball", "px"]
    strikesX <- pitchSample[pitchSample$des=="Called Strike", "px"]
    outDF[i,"rhIn"] <- inconsistency(ballsX, strikesX)
    outDF[i,"total"] <- sum(outDF[i,3:6]) }
  }
  return(outDF)
}

library(RCurl)
july2017games <- makeUrls("2017-04-01", "2017-08-22")
for(u in july2017games) {
  if (!url.exists(paste0(u,"/inning")))
    july2017games <- setdiff(july2017games, u)
}

july2017gids <- substr(july2017games, 66, 95)
#badgids <- c("gid_2017_07_05_nynmlb_wasmlb_1","gid_2017_04_03_detmlb_chamlb_1")
#badgids <- c("gid_2017_04_05_kcamlb_minmlb_1")
#july2017gids <- setdiff(july2017gids, badgids)
july2017umps <- makeInconsistencyDF(july2017gids)


