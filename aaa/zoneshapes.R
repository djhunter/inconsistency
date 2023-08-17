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

## Function to make contours for decision boundaries
zoneContourDF = function(b, s, cp, 
                         level=0.5, num_points = 200, limits = c(-2,2,0,5)) {
  StkKDE <- kde2d(s$px, s$pz, n = num_points, lims = limits)
  CpKDE <- kde2d(cp$px, cp$pz, n = num_points, lims = limits)
  CzKDE <- StkKDE
  CzKDE$z <- CzKDE$z/CpKDE$z*nrow(s)/nrow(cp)
  Szcontour <- contourLines(CzKDE, levels=level)
  Szcontourdf <- data.frame(px = Szcontour[[1]]$x, pz = Szcontour[[1]]$y)
  return(Szcontourdf)  
}

balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]
AAAABSzonedf <- zoneContourDF(balls, strikes, calledPitches)
AAAzone <- ggplot(AAAABSzonedf) +
  geom_path(aes(x=px, y=pz), color="black") +
  coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) +
  theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
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
MLBzone <- ggplot(zoneContourDF(MLBballs, MLBstrikes, MLBcalledPitches)) +
  geom_path(aes(x=px, y=pz), color="black") +
  coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) +
  theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  ggtitle("Human Umpire Zone (MLB)")
ggsave("docs/ABSvsMLBzones.png", plot = AAAzone + MLBzone, width = 16, height = 8, units = "cm")

cs_game_pbp <- all_game_pbp %>% # Challenge system in AAA
  filter(wday(game_date, label = TRUE) %in% c("Fri", "Sat", "Sun"))
cs_calledPitches <- cs_game_pbp %>%
  filter(details.code %in% c("B", "*B", "C"),
         !(is.na(pitchData.coordinates.pX) | is.na(pitchData.coordinates.pZ))) %>%
  transmute(px = pitchData.coordinates.pX, 
            pz = 2.0*(pitchData.coordinates.pZ - pitchData.strikeZoneTop)/
              (pitchData.strikeZoneTop - pitchData.strikeZoneBottom) + 3.5,
            des = details.description,
            stand = matchup.batSide.code) 
cs_balls <- calledPitches[calledPitches$des=="Ball" | calledPitches$des=="Ball In Dirt",
                       c("px", "pz", "stand")]
cs_strikes <- calledPitches[calledPitches$des=="Called Strike", c("px", "pz", "stand")]
AAACSzonedf <- zoneContourDF(cs_balls, cs_strikes, cs_calledPitches) %>%
  mutate(System = "Challenge")
allAAAzonedf <- AAAABSzonedf %>%
  mutate(System = "Automated") %>%
  bind_rows(AAACSzonedf)
ABSvsCS <- ggplot(allAAAzonedf) +
  geom_path(aes(x = px, y = pz, color = System)) +
  coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) +
  theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  ggtitle("ABS vs. Challenge System")

AAAzone <- ggplot(AAACSzonedf) +
  geom_path(aes(x=px, y=pz), color="black") +
  coord_fixed(xlim=c(-1.5,1.5), ylim=c(1.0,4)) +
  theme_bw() + theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  ggtitle("ABS Zone (AAA)")

