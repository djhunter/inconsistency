library(MASS)
library(baseballr)
library(tidyverse)
all_game_pbp_list <- readRDS("aaa/allAAAgames2023.rds") # scraped in getAAAdata.R
all_game_pbp <- bind_rows(all_game_pbp_list)
robot_game_pbp <- all_game_pbp %>%
  filter(wday(game_date, label = TRUE) %in% c("Tue", "Wed", "Thu"))

# normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
#calledPitches <- all_game_pbp %>%
calledPitches <- robot_game_pbp %>%
  filter(details.code %in% c("B", "*B", "C"),
         !(is.na(pitchData.coordinates.pX) | is.na(pitchData.coordinates.pZ))) %>%
  transmute(px = pitchData.coordinates.pX, 
            pz = 2.0*(pitchData.coordinates.pZ - pitchData.strikeZoneTop)/
              (pitchData.strikeZoneTop - pitchData.strikeZoneBottom) + 3.5,
            des = details.description,
            stand = matchup.batSide.code) 

balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]

# initialize L/R variables
stk <- list(L=tibble(), R=tibble())
bll <- list(L=tibble(), R=tibble())
cp <- list(L=tibble(), R=tibble())
stkKDE <- list(L=list(), R=list())
cpKDE <- list(L=list(), R=list())
czKDE <- list(L=list(), R=list())
szcontour <- list(L=list(), R=list())
szcontourdf <- list(L=data.frame(), R=data.frame())
for(s in c("L", "R")) {
  stk[[s]] <- strikes[strikes$stand==s,c("px","pz")]
  bll[[s]] <- balls[balls$stand==s,c("px","pz")]
  cp[[s]] <- calledPitches[calledPitches$stand==s,c("px","pz")]
  stkKDE[[s]] <- kde2d(stk[[s]]$px, stk[[s]]$pz, n=200, lims = c(-2,2,0,5))
  cpKDE[[s]] <- kde2d(cp[[s]]$px, cp[[s]]$pz, n=200, lims = c(-2,2,0,5))
  czKDE[[s]] <- stkKDE[[s]]
  czKDE[[s]]$z <- czKDE[[s]]$z/cpKDE[[s]]$z*nrow(stk[[s]])/nrow(cp[[s]])

  szcontour[[s]] <- contourLines(czKDE[[s]], levels=0.5)
  szcontourdf[[s]] <- data.frame(px = szcontour[[s]][[1]]$x, pz = szcontour[[s]][[1]]$y)
}

zoneShape <- list(L=list(), R=list())
for(s in c("L", "R")) {
   zoneShape[[s]] <- ggplot() + 
     geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="black") +
     coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) +
     theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
}

require(gridExtra)
grid.arrange(zoneShape$L, zoneShape$R)

# 
# conzones <- grid.arrange(strikePlot$L, strikePlot$R, ballPlot$L, ballPlot$R, ncol=2)
# ggsave("figures/consensus_zones.pdf", plot = conzones, width = 8, height = 8, dpi = 300)