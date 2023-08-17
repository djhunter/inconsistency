library(tidyverse)
library(patchwork)
all_game_pbp <- readRDS("aaa/allAAAgames2023.rds") %>% # scraped in getAAAdata.R
  bind_rows() %>%
  mutate(System = ifelse(wday(game_date, label = TRUE) %in% c("Tue", "Wed", "Thu"), 
                         "Automated", "Challenge"))

all_game_pbp %>%
  filter(details.code %in% c("B", "*B", "C")) %>%
  group_by(System) %>%
  summarize(CstrikePct = sum(details.code == "C")/n())

all_game_pbp %>%
  filter(details.code %in% c("B", "*B", "C")) %>%
  mutate(`Pitch Call` = factor(ifelse(details.code == "C", "Called Strike", "Ball"))) %>%
  select(`Pitch Call`, System) ->
  pitchcallsbysystem
chisq.test(pitchcallsbysystem$`Pitch Call`, pitchcallsbysystem$System)
prop.test(t(table(pitchcallsbysystem)))

summary(lm(as.numeric(`Pitch Call`) ~ System, data = pitchcallsbysystem))
summary(glm(`Pitch Call` ~ System, data = pitchcallsbysystem, family = "binomial"))

all_game_pbp %>%
  filter(details.code %in% c("B", "*B", "C")) %>%
  mutate(`Pitch Call` = factor(ifelse(details.code == "C", "Called Strike", "Ball"))) %>%
  glm(`Pitch Call` ~ System + pitchData.startSpeed + pitchData.breaks.spinRate, data = ., family = "binomial") %>%
  summary()

all_game_pbp %>%
  group_by(game_pk, atBatIndex) %>%
  summarize(result = result.event[1],
            System = System[1], .groups = "drop") %>%
  group_by(System) %>%
  summarize(`Walk Rate` = sum(result == "Walk")/n(),
            `Strikeout Rate` = sum(result == "Strikeout")/n(),
            `Hit Rate` = sum(result %in% c("Single", "Double", "Triple", "Home Run"))/n()) ->
  rateTable
rateTable

ABSvsCSrates <- rateTable %>%
  pivot_longer(cols = where(is.numeric), names_to = "Rate", values_to = "Proportion") %>%
  ggplot() +
    geom_bar(aes(x = Rate, y = Proportion, fill = System), stat = "identity", position = "dodge") +
    theme_bw() + 
    theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
    scale_fill_viridis_d(end = 0.7) +
    ggtitle("ABS vs. Challenge System Rates")
ggsave("docs/ABSvsCSrates.png", plot = ABSvsCSrates, width = 12, height = 8, units = "cm")  

all_game_pbp %>%
  group_by(game_pk, atBatIndex) %>%
  summarize(result = result.event[1],
            System = System[1]) %>%
  ungroup() %>%
  mutate(Walk = (result == "Walk")) %>% 
  select(System, Walk) ->
  walksbysystem
chisq.test(walksbysystem$Walk, walksbysystem$System)
prop.test(table(walksbysystem))
