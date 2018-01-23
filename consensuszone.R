library(ks)
library(ggplot2)

rs2017 <- readRDS("regSeason2017.Rda")
calledStrikes <- rs2017[rs2017$des=="Called Strike",]
normedz <- 2.0*(calledStrikes$pz-calledStrikes$sz_top)/(calledStrikes$sz_top-calledStrikes$sz_bot)+3.5
normedCalledStrikes <- calledStrikes
normedCalledStrikes$pz <- normedz
strikes <- normedCalledStrikes[,c("px","pz")]
strikes <- strikes[!is.na(strikes[,1]),]

# H.scv <- Hscv(x=strikes) # takes several hours
H.scv <- readRDS("HscvAllCalls.Rda")
#H.pi <- Hpi(x=strikes) # also tekes several hours, but faster
H.pi <- readRDS("HpiAllCalls.Rda")
fhat <- kde(x=strikes, H=H.scv, compute.cont=TRUE)
contour.05 <- with(fhat,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                     z=estimate,levels=cont["5%"])[[1]])
contour.10 <- with(fhat,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                     z=estimate,levels=cont["10%"])[[1]])
contour.15 <- with(fhat,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                     z=estimate,levels=cont["15%"])[[1]])
contour.20 <- with(fhat,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                     z=estimate,levels=cont["20%"])[[1]])
contourdf05 <- data.frame(px = contour.05$x, pz = contour.05$y)
contourdf10 <- data.frame(px = contour.10$x, pz = contour.10$y)
contourdf15 <- data.frame(px = contour.15$x, pz = contour.15$y)
contourdf20 <- data.frame(px = contour.20$x, pz = contour.20$y)
saveRDS(contourdf10, "upper90contourzone.Rda") # the consensus zone

ggplot() + geom_point(data=strikes, aes(x=px,y=pz), alpha=0.02, color="red3", size=3, stroke=1) +
           geom_path(data=contourdf05, aes(x=px, y=pz)) +
           geom_path(data=contourdf10, aes(x=px, y=pz)) +
           geom_path(data=contourdf15, aes(x=px, y=pz)) +
           geom_path(data=contourdf20, aes(x=px, y=pz)) +
           coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
           theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
ggsave("redstrikescontours.png", width=6, height=6, units="in", dpi = 300)


calledBalls <- rs2017[rs2017$des=="Ball",]
normedz <- 2.0*(calledBalls$pz-calledBalls$sz_top)/(calledBalls$sz_top-calledBalls$sz_bot)+3.5
normedCalledBalls <- calledBalls
normedCalledBalls$pz <- normedz
balls <- normedCalledBalls[,c("px","pz")]
balls <- balls[!is.na(balls[,1]),]

ggplot() + geom_point(data=balls, aes(x=px,y=pz), alpha=0.02, color="blue3", size=3, stroke=1) +
           geom_path(data=contourdf05, aes(x=px, y=pz)) +
           geom_path(data=contourdf10, aes(x=px, y=pz)) +
           geom_path(data=contourdf15, aes(x=px, y=pz)) +
           geom_path(data=contourdf20, aes(x=px, y=pz)) +
           coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
           theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
ggsave("blueballscontours.png", width=6, height=6, units="in", dpi = 300)

# Save the two images as png, then use imagemagick:
# composite -dissolve 50 -gravity Center redstrikescontours.png blueballscontours.png -alpha Set cloudcontours.png