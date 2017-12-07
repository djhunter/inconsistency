library(ggplot2)

rs2017 <- readRDS("regSeason2017.Rda")
calledStrikes <- rs2017[rs2017$des=="Called Strike",]
strikezone <- data.frame(x1=-0.83083,x2=0.83083,y1=1.5,y2=3.5)
ggplot()+geom_point(data=cs, aes(x=px, y=pz))+geom_rect(data=strikezone, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", alpha=0) +coord_fixed()

normedz <- 2.0*(calledStrikes$pz-calledStrikes$sz_top)/(calledStrikes$sz_top-calledStrikes$sz_bot)+3.5
normedCalledStrikes <- cbind(calledStrikes, normedz)
#TODO: redo plot with normed strikes, removing outliers, transparency