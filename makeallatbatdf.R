#alldata <- readRDS("all2017gamedata.Rda")
library(plyr)
n_games <- nrow(alldata)
#n_games <- 3 # for testing
#n_pitches <- 723687
# atbatbypitchdf <- data.frame(pitcher = numeric(n_pitches), 
#                              batter = numeric(n_pitches),
#                              num = numeric(n_pitches),
#                              b = numeric(n_pitches),
#                              s = numeric(n_pitches),
#                              o = numeric(n_pitches), 
#                              stand = character(n_pitches), 
#                              p_throws = character(n_pitches),
#                              event_num = numeric(n_pitches),
#                              home_team_runs = numeric(n_pitches),
#                              away_team_runs = numeric(n_pitches),
#                              inning = numeric(n_pitches),
#                              stringsAsFactors = FALSE)
abpl <- list()
k=1
for (i in 1:n_games) {
    playerdata <- alldata[i,]$player[[1]]
    gamedata <- alldata[i,]$game[[1]]
    pitchdata <- gamedata$pitch # all pitches for game
    atbatdata <- gamedata$atbat # all batter info for game
    n_pitchesingame <- nrow(pitchdata)
    for (j in 1:n_pitchesingame) {
      abpl[[k]] <- atbatdata[atbatdata$num==pitchdata[j,]$num,]
      # atbatbypitchdf[k,] <- list(r$pitcher, r$batter, r$num, r$b,
      #                            r$s, r$o, r$stand, r$p_throws, r$event_num,
      #                            r$home_team_runs, r$away_team_runs, r$inning)
      k = k + 1
    }
    if((i %% 10) == 0) cat(".")
    if((i %% 100) == 0) cat(" Processed", i, "games.\n")
}
cat("Finished constructing list.\n")
atbatbypitchdf <- rbind.fill(abpl)
#saveRDS(atbatbypitchdf, "atbatpitchdf2017.Rda")