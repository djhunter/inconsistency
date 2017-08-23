umpdata <- readRDS("aprilaugustumps.rds")
uumps <- unique(umpdata$ump)
umpAverages <- data.frame(ump = uumps, aveI = numeric(length(uumps)))
for (i in 1:length(uumps)) {
  umpAverages[i,"aveI"] = mean(umpdata[umpdata$ump==uumps[i], "total" ], na.rm=TRUE)
}