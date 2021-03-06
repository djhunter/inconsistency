library(alphahull)
library(ks)
library(pracma)

makeZoneSizeDF <- function(apdf, zeps=0.02) 
{
  umpIDs <- unique(apdf$umpID)
  n <- length(umpIDs)
  outDF <- data.frame(umpName = character(n),
                      umpID = umpIDs,
                      gamesCalled = numeric(n), 
                      zoneSizeRect = numeric(n), 
                      zoneSizeHull = numeric(n), 
                      zoneSizeKDE = numeric(n),
                      accuracyRect = numeric(n),
                      accuracyRectCon = numeric(n),
                      walkRate = numeric(n),
                      stringsAsFactors = FALSE)

  for (i in 1:n) {
    if((i %% 2) == 0) cat(".")
    if((i %% 20) == 0) cat(" Processed", i, "umpires.\n")
    pitchdata <- subset(apdf, umpID == umpIDs[i])
    outDF[i,"umpName"] <- as.character(pitchdata[1,"umpName"])
    # normalize up/down locations based on height of batter
    pitchdata$pz <- 2.0*(pitchdata$pz-pitchdata$sz_top)/(pitchdata$sz_top-pitchdata$sz_bot)+3.5
    strikes <- pitchdata[pitchdata$des=="Called Strike",c("px","pz")]
    strikes <- strikes[!is.na(strikes[,1]),]
    balls <- pitchdata[pitchdata$des=="Ball",c("px","pz")]
    balls <- balls[!is.na(balls[,1]),]
    xquant <- quantile(strikes$px, c(zeps, 1-zeps))
    zquant <- quantile(strikes$pz, c(zeps, 1-zeps))
    zhull <- ahull(subset(strikes, (px > xquant[1])&(px < xquant[2])&(pz>zquant[1])&(pz<zquant[2])), alpha=10000)
    outDF[i,"zoneSizeHull"] <- areaahull(zhull)
    outDF[i,"zoneSizeRect"] <- (xquant[2]-xquant[1])*(zquant[2]-zquant[1])
    outDF[i,"gamesCalled"] <- length(unique(pitchdata$gameday_link))
    
    missedS <- sum((strikes$pz < 1.5) |
                                         (strikes$pz > 3.5) |
                                         (strikes$px > 0.83083) |
                                         (strikes$px < -0.83083), na.rm = TRUE)
    missedB <- sum((balls$pz > 1.5) & 
                                       (balls$pz < 3.5) &
                                       (balls$px < 0.83083) &
                                       (balls$px > -0.83083), na.rm = TRUE)
    outDF[i,"accuracyRect"] <- 1-(missedS + missedB)/(nrow(balls) + nrow(strikes))
    missedS <- sum((strikes$pz < 1.4) |
                                         (strikes$pz > 3.5) |
                                         (strikes$px > 0.9) |
                                         (strikes$px < -0.9), na.rm = TRUE)
    missedB <- sum((balls$pz > 1.4) & 
                                       (balls$pz < 3.5) &
                                       (balls$px < 0.9) &
                                       (balls$px > -0.9), na.rm = TRUE)
    outDF[i,"accuracyRectCon"] <- 1-(missedS + missedB)/(nrow(balls) + nrow(strikes))
    playdata <- pitchdata[!duplicated(pitchdata$play_guid2),]
    num_walks <- sum(playdata$event == "Walk")
    outDF[i,"walkRate"] <- num_walks/nrow(playdata)
    H.scv <- Hscv(x=strikes)
    fhat <- kde(x=strikes, H=H.scv, compute.cont=TRUE)
    contour.05 <- with(fhat,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                        z=estimate,levels=cont["5%"])[[1]])
    outDF[i,"zoneSizeKDE"] <- abs(with(contour.05,polyarea(x,y)))
  }
  cat("Finished. Processed", i, "umpires.\n")
  return(outDF)
}

#rs2017 <- readRDS("regSeason2017.Rda")
cat("Finished reading data from file.", "\n")
colnames(rs2017)[69] <- "play_guid2"
colnames(rs2017) <- make.unique(colnames(rs2017))

#zumpDF <- makeZoneSizeDF(rs2017[1:2038,]) 
zumpDF <- makeZoneSizeDF(rs2017) 
zumpDF15plus <- subset(zumpDF, gamesCalled>=15)
# saveRDS(zumpDF, file="zumpDF2017.Rda")
# saveRDS(zumpDF15plus, file="zumpDF15plus2017.Rda")


# need to also run areaoverlap.R to do the following         # zeps=0.02
cor(umpAveIncon15plus$aveoverlap, zumpDF15plus$accuracyRect) # -0.45
cor(umpAveIncon15plus$aveincon, zumpDF15plus$accuracyRect)   # -0.55
cor(zumpDF15plus$zoneSizeHull, zumpDF15plus$accuracyRect)    # -0.77
cor(zumpDF15plus$zoneSizeRect, zumpDF15plus$accuracyRect)    # -0.77
cor(umpAveIncon15plus$aveincon, zumpDF15plus$accuracyRectCon) # -0.65
cor(umpAveIncon15plus$aveoverlap, zumpDF15plus$accuracyRectCon) # -0.53
cor(umpAveIncon15plus$aveincon, zumpDF15plus$walkRate)       # -0.65

zumpDF15plus$aveincon <- umpAveIncon15plus$aveincon
# ggplot(zumpDF15plus, aes(y=accuracyRect, x=aveincon, label=umpName))+
#   geom_point() +geom_text(aes(label=umpName),hjust=-0.05, vjust=0, size=4)+
#   labs(y="Rule Book Accuracy", x = "Average Inconsistency Index")+
#   theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20))
# 
# ggplot(zumpDF15plus, aes(y=accuracyRectCon, x=aveincon, label=umpName))+
#   geom_point() +geom_text(aes(label=umpName),hjust=0.5, vjust=-0.31, size=5)+
#   labs(y="Consensus Rule Book Accuracy", x = "Average Inconsistency Index")+
#   theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20))
# ggsave("umpscatter3.pdf", width=18, height=9)

umpMetrics <- cbind(zumpDF15plus, umpAveIncon15plus$aveoverlap)
colnames(umpMetrics) <- c("Name", "ID", "games", "zSizeR", "zSizeH", "zSizeK", "accR", "accCR", "walkRt", "inconI", "inconA")
umpMetrics <- umpMetrics[,c("games", "zSizeK","accCR","walkRt","inconI")]
colnames(umpMetrics) <- c("Games","ZoneSize","Accuracy","WalkRate","AveIncon")
saveRDS(umpMetrics,file="metricsforslides.Rda")

# rownames(umpMetrics) <- umpMetrics[,1]
# umpMetrics[,1:2] <- NULL
round(cor(umpMetrics),2)
# pcaUmpMetrics = with(umpMetrics, data.frame(zSizeR=zSizeR, zSizeH=zSizeH, accR=accR, 
#                                             accCR = accCR, conI = 1-inconI, conA=1-inconA))
# rownames(pcaUmpMetrics) <- rownames(umpMetrics)
# prcomp(subset(umpMetrics,select=-games),scale=TRUE)
# prcomp(pcaUmpMetrics, scale = TRUE)
