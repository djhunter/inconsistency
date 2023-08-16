library(MASS)
library(baseballr)
library(tidyverse)
library(patchwork)
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

allAAAcalls <- calledPitches %>% 
  filter(des != "Ball In Dirt") %>%
  ggplot(aes(x = px, y = pz, color = des)) +
  coord_fixed(xlim=c(-2,2), ylim=c(0,5)) + 
  geom_point(size = 4, shape = 1, alpha = 0.5) +
  theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  guides(color = guide_legend(title = "Pitch Result")) +
  scale_color_manual(values = c("#355E3B", "#AA0000"))
ggsave("docs/allAAAcalls.png", plot = allAAAcalls, width = 12, height = 10, units = "cm")  

## Function to make contour plots for decision boundaries
zoneContourPlot = function(b, s, cp, color = "black") {
  StkKDE <- kde2d(s$px, s$pz, n = 200, lims = c(-2,2,0,5))
  CpKDE <- kde2d(cp$px, cp$pz, n = 200, lims = c(-2,2,0,5))
  CzKDE <- StkKDE
  CzKDE$z <- CzKDE$z/CpKDE$z*nrow(s)/nrow(cp)
  Szcontour <- contourLines(CzKDE, levels=0.5)
  Szcontourdf <- data.frame(px = Szcontour[[1]]$x, pz = Szcontour[[1]]$y)
  ZoneShape <- ggplot() +
    geom_path(data=Szcontourdf, aes(x=px, y=pz), color=color) +
    coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) +
    theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
  return(ZoneShape)  
}

balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]
AAAzone <- zoneContourPlot(balls, strikes, calledPitches) +
  ggtitle("ABS Zone (AAA)")

## Now repeat for MLB season
MLBcalledPitches <- readRDS("aaa/allMLBgames2023.rds") %>% # scraped in getMLBdata.R
  bind_rows() %>%
  filter(details.code %in% c("B", "*B", "C"),
         !(is.na(pitchData.coordinates.pX) | is.na(pitchData.coordinates.pZ))) %>%
  transmute(px = pitchData.coordinates.pX, 
            pz = 2.0*(pitchData.coordinates.pZ - pitchData.strikeZoneTop)/
              (pitchData.strikeZoneTop - pitchData.strikeZoneBottom) + 3.5,
            des = details.description,
            stand = matchup.batSide.code) 
MLBballs <- MLBcalledPitches[MLBcalledPitches$des=="Ball" | MLBcalledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
MLBstrikes <- MLBcalledPitches[MLBcalledPitches$des=="Called Strike", c("px", "pz", "stand")]
MLBzone <- zoneContourPlot(MLBballs, MLBstrikes, MLBcalledPitches) +
  ggtitle("Human Umpire Zone (MLB)")

ggsave("docs/ABSvsMLBzones.png", plot = AAAzone + MLBzone, width = 16, height = 8, units = "cm")

