library(pitchRx)
library(alphahull)
library(geometry)

gameID <- "gid_2017_08_12_chnmlb_arimlb_1"
playerdata <- scrape(game.ids=gameID, suffix="players.xml")
umpName <- playerdata$umpire[playerdata$umpire$position=="home", "name"]
umpID <- playerdata$umpire[playerdata$umpire$position=="home", "id"]

gamedata <- scrape(game.ids=gameID)
pitchdata <- gamedata$pitch # all pitches
atbatdata <- gamedata$atbat
numL <- atbatdata[atbatdata$stand=="L","num"]
numR <- atbatdata[atbatdata$stand=="R","num"]
Lpitchdata <- pitchdata[pitchdata$num %in% numL,]
Rpitchdata <- pitchdata[pitchdata$num %in% numR,]
Lballs <- Lpitchdata[Lpitchdata$des=="Ball",c("px","pz")]
Lstrikes <- Lpitchdata[Lpitchdata$des=="Called Strike",c("px","pz")]
Rballs <- Rpitchdata[Rpitchdata$des=="Ball",c("px","pz")]
Rstrikes <- Rpitchdata[Rpitchdata$des=="Called Strike",c("px","pz")]

plot(Rballs,xlim=c(-4,4),ylim=c(0,6))
title(main=paste(c(umpName, gameID, " vs. Right-handed batters")))
points(Rstrikes,col="red",pch=5)

plot(Lballs,xlim=c(-4,4),ylim=c(0,6))
title(main=paste(c(umpName, gameID, " vs. Left-handed batters")))
points(Lstrikes,col="red",pch=5)
LstrikeHull <- ahull(Lstrikes, alpha=1)
lines(Lstrikes[c(LstrikeHull$arcs[,7],LstrikeHull$arcs[1,7]),],col=2)

