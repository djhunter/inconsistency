# tables and other calculations used in the paper

library(dplyr)
library(tibble)
library(xtable)

games17inc <- as_data_frame(readRDS("games17inc.Rda"))
games17inc <- subset(games17inc, npitch>=50) # throw out games with less than 50 pitches
games17inc <- subset(games17inc, npitch<=300) # throw out games with more than 300 pitches
xtable(cor(games17inc[,c(5,6,8,9)]))

summary(games17inc$incR10+games17inc$incACH7)
sd(games17inc$incR10+games17inc$incACH7)
hist(games17inc$incR10+games17inc$incACH7)

compare_alpha <- readRDS("compare_alpha.Rda")
apply(compare_alpha[,c(5:11)], 2, mean)
apply(compare_alpha[,c(5:11)], 2, sd)
xtable(cor(compare_alpha[,c(6:11)]))

umps17 <- readRDS("umps17.Rda")
regularUmps17 <- umps17[umps17$ngames >= 20,]
