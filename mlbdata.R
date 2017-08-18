library("pitchRx")
inconsistency <- function(A,B) {
  pairs <- expand.grid(A,B)
  diffs <- pairs[,1]-pairs[,2]
  Iab <- 1/(length(A)*length(B))*sum(diffs[diffs>0])
  return(Iab/(mean(B)-mean(A)))
}

#gamedata <- scrape(game.ids = "gid_2017_08_16_cinmlb_chnmlb_1")
#gamedata <- scrape(game.ids = "gid_2017_08_15_cinmlb_chnmlb_1")
gamedata <- scrape(game.ids = "gid_2017_08_12_chnmlb_arimlb_1")
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

#Rpitchdata <- Rpitchdata[(Rpitchdata$px>0)&(Rpitchdata$pz>1.5)&(Rpitchdata$pz<3.0),]
Lballs <- Lpitchdata[Lpitchdata$des=="Ball",c("px","pz")]
LcalledStrikes <- Lpitchdata[Lpitchdata$des=="Called Strike",c("px","pz")]
plot(Lballs)
points(LcalledStrikes,col="red")

Rballs <- Rpitchdata[Rpitchdata$des=="Ball",c("px","pz")]
RcalledStrikes <- Rpitchdata[Rpitchdata$des=="Called Strike",c("px","pz")]
plot(Rballs)
points(RcalledStrikes,col="red")

