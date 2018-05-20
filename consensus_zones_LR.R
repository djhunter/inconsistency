library(ks)
library(ggplot2)
library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

strikes <- pitches[pitches$des=="Called Strike",c("px","pz","stand","sz_top", "sz_bot")]
# normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
strikes$pz <- 2.0*(strikes$pz-strikes$sz_top)/(strikes$sz_top-strikes$sz_bot)+3.5
strikes <- strikes[!is.na(strikes[,1]),]

#strikes <- strikes[1:100,] # for testing

strikesL <- strikes[strikes$stand=="L",c("px","pz")]
strikesR <- strikes[strikes$stand=="R",c("px","pz")]

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
