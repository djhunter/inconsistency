library(tidyverse)
library(lubridate)
library(knitr)
library(MASS)
library(gridExtra)

# The baseballr package wasn't working, so use the source code:
source('~/baseballr/R/get_pbp_mlb.R')
source('~/baseballr/R/get_game_pks_mlb.R')
load("~/baseballr/data/stats_api_live_empty_df.rda")

dog <- today()
dog <- "2020-07-29"

pks <- get_game_pks_mlb(dog)

teams <- cbind(pks$teams.away.team.name, pks$teams.home.team.name)
colnames(teams) <- c("Away Team", "Home Team")
kable(teams)

gamenum <- which(pks$teams.away.team.name == "Chicago Cubs" | pks$teams.home.team.name == "Chicago Cubs")

gamedata <- get_pbp_mlb(pks$game_pk[gamenum]) 

calledPitches <- gamedata %>%
  filter(details.code %in% c("B", "*B", "C")) %>%
  transmute(px = pitchData.coordinates.pX, 
            pz = pitchData.coordinates.pZ,
            des = details.description,
            stand = matchup.batSide.code) %>%
  filter(!(is.na(px) | is.na(pz)))
npitch <- nrow(calledPitches)
    
balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                         c("px", "pz", "stand")]
strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]
    
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
  czKDE[[s]]$z <- czKDE[[s]]$z/cpKDE[[s]]$z*nrow(stk$L)/nrow(cp$L)
  
  szcontour[[s]] <- contourLines(czKDE[[s]], levels=0.5)
  szcontourdf[[s]] <- data.frame(px = szcontour[[s]][[1]]$x, pz = szcontour[[s]][[1]]$y)
}
    
strikePlot <- list(L=list(), R=list())
   
s <- "L"
strikePlot[[s]] <- ggplot() + 
                     geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="red3") +
                     geom_point(data=bll[[s]], aes(x=px,y=pz), alpha=0.3, color="blue", size=3, stroke=1) +
                     geom_point(data=stk[[s]], aes(x=px,y=pz), alpha=0.3, color="red3", size=3, stroke=1, shape=23, fill="red3") +
                     coord_fixed(xlim=c(-2,2), ylim=c(0,5)) + 
                     theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
                     ggtitle("vs. left-handed batters")
s <- "R"
strikePlot[[s]] <- ggplot() + 
                     geom_path(data=szcontourdf[[s]], aes(x=px, y=pz), color="red3") +
                     geom_point(data=bll[[s]], aes(x=px,y=pz), alpha=0.3, color="blue", size=3, stroke=1) +
                     geom_point(data=stk[[s]], aes(x=px,y=pz), alpha=0.3, color="red3", size=3, stroke=1, shape=23, fill="red3") +
                     coord_fixed(xlim=c(-2,2), ylim=c(0,5)) + 
                     theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
                     ggtitle("vs. right-handed batters")
umpzones <- grid.arrange(strikePlot$L, strikePlot$R, ncol=2)
