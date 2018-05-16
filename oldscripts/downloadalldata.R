library("pitchRx")

getAllData <- function(gameIDs) 
{
  n <- length(gameIDs)
  hit <- list()
  player <- list()
  game <- list()
  
  for (i in 1:n) {
    h <- scrape(game.ids=gameIDs[i], suffix="inning/inning_hit.xml")
    if ( is.null(nrow(h$hip))){
      hit[[i]] <- NULL
      player[[i]] <- NULL
      game[[i]] <- NULL
      cat("bad gid: ", gameIDs[i], "\n")
    }
    else  { 
    hit[[i]] <- h
    player[[i]] <- scrape(game.ids=gameIDs[i], suffix="players.xml")
    game[[i]] <- scrape(game.ids=gameIDs[i])
    }
  }
  outDF = data.frame(gameIDs, I(hit), I(player), I(game))
  return(outDF)
}

library(RCurl)

### get game ids from url
games <- makeUrls("2017-04-01", "2017-10-02") # 2017 regular season
#games <- makeUrls("2017-04-03", "2017-04-03") # for testing
for(u in games) {
  if ((!url.exists(paste0(u,"/inning"))) | (!url.exists(paste0(u,"/emailSource.xml"))))
    games <- setdiff(games, u)
}

gids <- substr(games, 66, 95)
### done getting game ids

#gids <- "gid_2017_07_01_bosmlb_tormlb_1"
#badgids <- c("gid_2017_07_05_nynmlb_wasmlb_1","gid_2017_04_03_detmlb_chamlb_1") 
#badgids <- c("gid_2017_04_05_kcamlb_minmlb_1")
#july2017gids <- setdiff(july2017gids, badgids)
#gids <- c("gid_2017_04_03_detmlb_chamlb_1") 
allDF <- getAllData(gids)

#saveRDS(allDF, file="all2017gamedata.Rda")
saveRDS(allDF, file="temp.Rda")
