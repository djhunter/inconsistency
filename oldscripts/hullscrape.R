library("pitchRx")
library("alphahull")

makeHullDF <- function(gameIDs) 
{
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
                      stringsAsFactors = FALSE)
  
  for (i in 1:n) {
    hitdata <- scrape(game.ids=gameIDs[i], suffix="inning/inning_hit.xml")
    if ( is.null(nrow(hitdata$hip))){
      outDF[i,"ump"] <- "<Rainout>"
      outDF[i,"total"] <- NA
    }
    else  { 
    playerdata <- scrape(game.ids=gameIDs[i], suffix="players.xml")
    outDF[i,"umpName"] <- playerdata$umpire[playerdata$umpire$position=="home", "name"]
    outDF[i,"umpID"] <- playerdata$umpire[playerdata$umpire$position=="home", "id"]
    gamedata <- scrape(game.ids=gameIDs[i])
    pitchdata <- gamedata$pitch # all pitches
    # normalize up/down locations based on height of batter
    if(!is.null(pitchdata$pz)) {
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
    emptyhull <- ahull(c(-100, -101, -100), c(0,0,1), alpha=10000) # kludge: not really empty but no data will ever be in it
    if (nrow(Rstrikes)>2) RstrikeHull <- ahull(Rstrikes, alpha=10000) else RstrikeHull <- emptyhull
    if (nrow(Lstrikes)>2) LstrikeHull <- ahull(Lstrikes, alpha=10000) else LstrikeHull <- emptyhull
    alpha = 0.6 # could pass as parameter: bigger seems less fair to umpires
    if (nrow(Rballs)>2) RballHull <- ahull(Rballs, alpha=alpha) else RballHull <- emptyhull
    if (nrow(Lballs)>2) LballHull <- ahull(Lballs, alpha=alpha) else LballHull <- emptyhull
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
    } }
  }
  return(outDF)
}

library(RCurl)
games <- makeUrls("2017-04-01", "2017-10-02")
for(u in games) {
  if (!url.exists(paste0(u,"/inning")))
    games <- setdiff(games, u)
}

gids <- substr(games, 66, 95)
#gids <- "gid_2017_07_01_bosmlb_tormlb_1"
#badgids <- c("gid_2017_07_05_nynmlb_wasmlb_1","gid_2017_04_03_detmlb_chamlb_1")
#badgids <- c("gid_2017_04_05_kcamlb_minmlb_1")
#july2017gids <- setdiff(july2017gids, badgids)
umpDF <- makeHullDF(gids)

umpDF <- umpDF[umpDF$totalCalls>50,] # eliminate small samples
uumpids <- unique(umpDF$umpID)
umpAveIncon <- data.frame(ID = uumpids, name=character(length(uumpids)), 
                          aveincon = numeric(length(uumpids)),
                          stringsAsFactors = FALSE)
for (i in 1:length(uumpids)) {
  umpAveIncon[i,"aveincon"] = mean(umpDF[umpDF$umpID==uumpids[i], "inconIdx" ], na.rm=TRUE)
  umpAveIncon[i,"name"] = umpDF[umpDF$umpID==uumpids[i],"umpName"][1]
}
sortedumps <- umpAveIncon[order(umpAveIncon$aveincon),]
print(sortedumps[,c("name","aveincon")], row.names = FALSE, right=FALSE)

saveRDS(umpDF, file="regularseason2017.Rda")
