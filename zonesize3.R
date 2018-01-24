library(sp)

umpdf <- readRDS("zumpDF15plus2017.Rda")
apdf <- readRDS("regSeason2017.Rda")
conzoneKDE <- readRDS("upper90contourzone.Rda")
umpIDs <- umpdf$umpID
n <- length(umpIDs)
accKDE <- numeric(n)

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
    totalCalls <- nrow(strikes)+nrow(balls)
    badBalls <- sum(point.in.polygon(balls$px, balls$pz, conzoneKDE$px, conzoneKDE$pz) != 0)
    badStrikes <- sum(point.in.polygon(strikes$px, strikes$pz, conzoneKDE$px, conzoneKDE$pz) == 0)
    accKDE[i] <- (totalCalls-badBalls-badStrikes)/totalCalls
}
cat("Finished. Processed", i, "umpires.\n")

# umpdf$accKDE <- accKDE

# slidemetrics <- readRDS("metricsforslides.Rda")

# umpdf$Krate <- slidemetrics$Krate
# saveRDS(umpdf, "umpdf2017.Rda")

# Figures for the paper:
# xtable(cor(umpdf[,c(11,7,8,6)]))

pca <- prcomp(umpdf[,c(6,11,9,12,10)],scale=TRUE)

# Figures for the paper:
# xtable(cor(umpdf[,c(11,7,8,6)]))
# xtable(pca$rotation)
# xtable(cor(umpdf[,c(6,11,9,12,10)]))
