library(alphahull)
library(ks)
library(pracma)

makeZoneSize2DF <- function(apdf) 
{
  umpIDs <- unique(apdf$umpID)
  n <- length(umpIDs)
  outDF <- data.frame(umpID = umpIDs,
                      StrikeoutRate = numeric(n),
                      stringsAsFactors = FALSE)

  for (i in 1:n) {
    if((i %% 2) == 0) cat(".")
    if((i %% 20) == 0) cat(" Processed", i, "umpires.\n")
    pitchdata <- subset(apdf, umpID == umpIDs[i])
    # normalize up/down locations based on height of batter
    pitchdata$pz <- 2.0*(pitchdata$pz-pitchdata$sz_top)/(pitchdata$sz_top-pitchdata$sz_bot)+3.5
    strikes <- pitchdata[pitchdata$des=="Called Strike",c("px","pz")]
    strikes <- strikes[!is.na(strikes[,1]),]
    balls <- pitchdata[pitchdata$des=="Ball",c("px","pz")]
    balls <- balls[!is.na(balls[,1]),]
    
    playdata <- pitchdata[!duplicated(pitchdata$play_guid2),]
    num_ks <- sum(playdata$event == "Strikeout")
    outDF[i,"StrikeoutRate"] <- num_ks/nrow(playdata)
  }
  cat("Finished. Processed", i, "umpires.\n")
  return(outDF)
}

#rs2017 <- readRDS("regSeason2017.Rda")
#cat("Finished reading data from file.", "\n")
#colnames(rs2017)[69] <- "play_guid2"
#colnames(rs2017) <- make.unique(colnames(rs2017))

zumpDF <- readRDS("zumpDF2017.Rda")
zumpDF2 <- makeZoneSize2DF(rs2017)
zumpDF$StrikeoutRate <- zumpDF2$StrikeoutRate
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

umpMetrics <- zumpDF15plus[,c("gamesCalled","zoneSizeKDE","accuracyRectCon","walkRate","StrikeoutRate","aveincon" )]
colnames(umpMetrics) <- c("Games","ZoneSize","Accuracy","BBrate","Krate","Incon")
saveRDS(umpMetrics,file="metricsforslides.Rda")

# rownames(umpMetrics) <- umpMetrics[,1]
# umpMetrics[,1:2] <- NULL
#round(cor(umpMetrics),2)
# pcaUmpMetrics = with(umpMetrics, data.frame(zSizeR=zSizeR, zSizeH=zSizeH, accR=accR, 
#                                             accCR = accCR, conI = 1-inconI, conA=1-inconA))
# rownames(pcaUmpMetrics) <- rownames(umpMetrics)
# prcomp(subset(umpMetrics,select=-games),scale=TRUE)
# prcomp(pcaUmpMetrics, scale = TRUE)
library(ggfortify)
rownames(umpMetrics) <- zumpDF15plus$umpName
pca <- prcomp(subset(umpMetrics,select=c(-Games)),scale=TRUE)
print(pca, digits=2)
autoplot(pca, label=TRUE, label.size=5, label.hjust=-0.05, xlab = "Strike Zone Sloppiness", ylab = "Hitter Friendliness")+theme(axis.text=element_text(size=12),axis.title=element_text(size=16,face="bold"))
ggsave("umpPCAscatter.pdf", width=18, height=9)
