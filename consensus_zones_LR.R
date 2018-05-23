library(ks)
library(sp)
library(ggplot2)
library(dplyr)
library(tibble)
pitches <- as_data_frame(readRDS("pitches2017.Rda"))

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

# initialize L/R variables
stk <- list(L=data_frame(), R=data_frame())
bll <- list(L=data_frame(), R=data_frame())
H_scv <- list(L = matrix(), R = matrix())
H_pi <- list(L = matrix(), R = matrix())
fhat <- list(L=list(), R=list())
szcontour <- list(L=list(), R=list())
szcontourdf <- list(L=data.frame(), R=data.frame())
strikesincontour <- list(L=numeric(), R=numeric())
ballsincontour <- list(L=numeric(), R=numeric())
for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  bll[[s]] <- balls[balls$stand==s,c("px","pz")]
  H_scv[[s]] <- Hscv(x=stk[[s]])
  H_pi[[s]] <- Hpi(x=stk[[s]])
  fhat[[s]] <- kde(x=stk[[s]], H=H_scv[[s]], compute.cont=TRUE)
  szcontour[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                    z=estimate,levels=cont["10%"])[[1]])
  szcontourdf[[s]] <- data.frame(px = szcontour[[s]]$x, pz = szcontour[[s]]$y)
  strikesincontour[[s]] <- sum(point.in.polygon(stk[[s]]$px, stk[[s]]$pz, 
                                                szcontourdf[[s]]$px, szcontourdf[[s]]$pz) != 0)
  ballsincontour[[s]] <- sum(point.in.polygon(bll[[s]]$px, bll[[s]]$pz, 
                                                szcontourdf[[s]]$px, szcontourdf[[s]]$pz) != 0)
}

saveRDS(H_scv, "hscvLR2017.Rda")

# print("proportion of Lstrikes in contour = ", Lstrikesincontour/nrow(strikesL)) # should be 0.90
# print("proportion of Rstrikes in contour = ", Rstrikesincontour/nrow(strikesR)) # should be 0.90
# print("proportion of Lballs in contour = ", Lballsincontour/nrow(ballsL)) # should be 0.10
# print("proportion of Rballs in contour = ", Rballsincontour/nrow(ballsR)) # should be 0.10
# 
# 
# strike_contour_plotL <- ggplot() + geom_point(data=strikesL, aes(x=px,y=pz), alpha=0.02, color="red3", size=3, stroke=1) +
#            geom_path(data=contourdf10L, aes(x=px, y=pz), color="black") +
#            coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
#            theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
# strike_contour_plotR <- ggplot() + geom_point(data=strikesR, aes(x=px,y=pz), alpha=0.02, color="red3", size=3, stroke=1) +
#            geom_path(data=contourdf10R, aes(x=px, y=pz), color="black") +
#            coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
#            theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
# 
# require(gridExtra)
# grid.arrange(strike_contour_plotL, strike_contour_plotR, ncol=2)
