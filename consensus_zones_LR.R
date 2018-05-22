library(ks)
library(ggplot2)
library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

strikes <- pitches[pitches$des=="Called Strike",c("px","pz","stand","sz_top", "sz_bot")]
# normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
strikes$pz <- 2.0*(strikes$pz-strikes$sz_top)/(strikes$sz_top-strikes$sz_bot)+3.5
strikes <- strikes[!is.na(strikes[,1]),]

balls <- pitches[pitches$des=="Ball" | pitches$des=="Ball In Dirt",
                 c("px","pz","stand","sz_top", "sz_bot")]
# normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
balls$pz <- 2.0*(balls$pz-balls$sz_top)/(balls$sz_top-balls$sz_bot)+3.5
balls <- balls[!is.na(balls[,1]),]
#strikes <- strikes[1:100,] # for testing
stk <- list(L=data.frame(), R=data.frame())
bll <- list(L=data.frame(), R=data.frame())

for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  bll[[s]] <- balls[balls$stand==s,c("px","pz")]
}

H_scv_L <- Hscv(x=strikesL) 
H_scv_R <- Hscv(x=strikesR) 
H_pi_L <- Hpi(x=strikesL) 
H_pi_R <- Hpi(x=strikesR) 

fhatL <- kde(x=strikesL, H=H_scv_L, compute.cont=TRUE)
fhatR <- kde(x=strikesR, H=H_scv_R, compute.cont=TRUE)
contour.10L <- with(fhatL,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                     z=estimate,levels=cont["10%"])[[1]])
contour.10R <- with(fhatR,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                     z=estimate,levels=cont["10%"])[[1]])
contourdf10L <- data.frame(px = contour.10L$x, pz = contour.10L$y)
contourdf10R <- data.frame(px = contour.10R$x, pz = contour.10R$y)

Lstrikesincontour <- sum(point.in.polygon(strikesL$px, strikesL$pz, contourdf10L$px, contourdf10L$pz) != 0)
print("proportion of Lstrikes in contour = ", Lstrikesincontour/nrow(strikesL)) # should be 0.90
Rstrikesincontour <- sum(point.in.polygon(strikesR$px, strikesR$pz, contourdf10R$px, contourdf10R$pz) != 0)
print("proportion of Rstrikes in contour = ", Rstrikesincontour/nrow(strikesR)) # should be 0.90
Lballsincontour <- sum(point.in.polygon(ballsL$px, ballsL$pz, contourdf10L$px, contourdf10L$pz) != 0)
print("proportion of Lballs in contour = ", Lballsincontour/nrow(ballsL)) # should be 0.10
Rballsincontour <- sum(point.in.polygon(ballsR$px, ballsR$pz, contourdf10R$px, contourdf10R$pz) != 0)
print("proportion of Rballs in contour = ", Rballsincontour/nrow(ballsR)) # should be 0.10


strike_contour_plotL <- ggplot() + geom_point(data=strikesL, aes(x=px,y=pz), alpha=0.02, color="red3", size=3, stroke=1) +
           geom_path(data=contourdf10L, aes(x=px, y=pz), color="black") +
           coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
           theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
strike_contour_plotR <- ggplot() + geom_point(data=strikesR, aes(x=px,y=pz), alpha=0.02, color="red3", size=3, stroke=1) +
           geom_path(data=contourdf10R, aes(x=px, y=pz), color="black") +
           coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
           theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())

require(gridExtra)
grid.arrange(strike_contour_plotL, strike_contour_plotR, ncol=2)
