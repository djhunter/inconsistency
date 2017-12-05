#allpitchumpdata <- readRDS("all2017pitchump.Rda")

umpids <- unique(allpitchumpdata$umpID)
# umpids <- 427269 # for testing
n <- length(umpids)
aggUmpDF <- data.frame(umpName = character(n), 
                      umpID = numeric(n),
                      CSinBallHull = numeric(n), 
                      CBinStrikeHull = numeric(n), 
                      totalCalls = numeric(n), 
                      calledStrikes = numeric(n),
                      calledBalls = numeric(n),
                      aggInconIdx = numeric(n),
                      missedRBStrikes = numeric(n),
                      missedRBBalls = numeric(n),
                      accuracy = numeric(n),
                      stringsAsFactors = FALSE)
                       
for (i in 1:n) {
  uid <- umpids[i]
  umpname <- allpitchumpdata[allpitchumpdata$umpID == uid,]$umpName[1]
  numpitches <- sum(allpitchumpdata$umpID == uid)
  pitchdata <- allpitchumpdata[allpitchumpdata$umpID == uid,]
  balls <- pitchdata[pitchdata$des=="Ball", c("px", "pz", "sz_top", "sz_bot")]
  strikes <- pitchdata[pitchdata$des=="Called Strike",c("px", "pz", "sz_top", "sz_bot")]
  
  aggUmpDF[i,"umpName"] <- umpname
  aggUmpDF[i,"umpID"] <- uid
  aggUmpDF[i,"calledStrikes"] <- nrow(strikes)
  aggUmpDF[i,"calledBalls"] <- nrow(balls)
  missedS <- sum((strikes$pz < strikes$sz_bot) |
                                       (strikes$pz > strikes$sz_top) |
                                       (strikes$px > 0.83083) |
                                       (strikes$px < -0.83083), na.rm = TRUE)
  aggUmpDF[i,"missedRBStrikes"] <- missedS
  missedB <- sum((balls$pz > balls$sz_bot) & 
                                     (balls$pz < balls$sz_top) &
                                     (balls$px < 0.83083) &
                                     (balls$px > -0.83083), na.rm = TRUE)
  aggUmpDF[i,"missedRBBalls"] <- missedB
  aggUmpDF[i,"totalCalls"] <- aggUmpDF[i,"calledStrikes"] + aggUmpDF[i,"calledBalls"]
  aggUmpDF[i,"accuracy"] <- (aggUmpDF[i,"totalCalls"] - (missedB + missedS))/aggUmpDF[i,"totalCalls"]
}

sortedumps <- aggUmpDF[order(aggUmpDF$accuracy, decreasing=TRUE),]
print(sortedumps[,c("umpName", "accuracy")], row.names=FALSE, right=FALSE)
