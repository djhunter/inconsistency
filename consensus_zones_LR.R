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
H_scv <- readRDS("hscvLR2017.Rda") # read these instead of
H_pi <- readRDS("hpiLR2017.Rda")   # recomputing
for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  bll[[s]] <- balls[balls$stand==s,c("px","pz")]
  # H_scv[[s]] <- Hscv(x=stk[[s]]) # takes several hours
  # H_pi[[s]] <- Hpi(x=stk[[s]]) #takes hours
  fhat[[s]] <- kde(x=stk[[s]], H=H_scv[[s]], compute.cont=TRUE)
  szcontour[[s]] <- with(fhat[[s]], contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                    z=estimate,levels=cont["10%"])[[1]])
  szcontourdf[[s]] <- data.frame(px = szcontour[[s]]$x, pz = szcontour[[s]]$y)
  strikesincontour[[s]] <- sum(point.in.polygon(stk[[s]]$px, stk[[s]]$pz, 
                                                szcontourdf[[s]]$px, szcontourdf[[s]]$pz) != 0)
  ballsincontour[[s]] <- sum(point.in.polygon(bll[[s]]$px, bll[[s]]$pz, 
                                                szcontourdf[[s]]$px, szcontourdf[[s]]$pz) != 0)
}

#saveRDS(H_scv, "hscvLR2017.Rda")
#saveRDS(H_pi, "hpiLR2017.Rda")

cat("proportion of Lstrikes in contour =", strikesincontour$L/nrow(stk$L), "\n")
cat("proportion of Rstrikes in contour =", strikesincontour$R/nrow(stk$R), "\n")
cat("proportion of Lballs in contour =", ballsincontour$L/nrow(bll$L), "\n")
cat("proportion of Rballs in contour =", ballsincontour$R/nrow(bll$R), "\n")

strikePlot <- list(L=list(), R=list())
ballPlot <- list(L=list(), R=list())
 
for(s in c("L", "R")) {
  strikePlot[[s]] <- ggplot() + geom_point(data=stk[[s]], aes(x=px,y=pz), alpha=0.01, color="red3", size=3, stroke=1) +
                     geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="black") +
                     coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
                     theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  ballPlot[[s]] <- ggplot() + geom_point(data=bll[[s]], aes(x=px,y=pz), alpha=0.01, color="blue", size=3, stroke=1) +
                   geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="black") +
                   coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) + 
                   theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
}
require(gridExtra)
conzones <- grid.arrange(strikePlot$L, strikePlot$R, ballPlot$L, ballPlot$R, ncol=2)
ggsave("figures/consensus_zones.pdf", plot = conzones, width = 8, height = 8, dpi = 300)

# to reduce size:
# pdf2ps consensus_zones.pdf consensus_zones.eps
# ps2pdf -dPDFSETTINGS=/printer consensus_zones.eps consensus_zones_printer.pdf

