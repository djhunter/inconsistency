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

H_scv_L <- Hscv(x=strikesL) # takes several hours
saveRDS(H_scv_L, "h_scv_l.Rda")
H_scv_R <- Hscv(x=strikesR) # takes several hours
saveRDS(H_scv_R, "h_scv_r.Rda")
H_pi_L <- Hpi(x=strikesL) # also tekes several hours, but faster
saveRDS(H_pi_L, "h_pi_l.Rda")
H_pi_R <- Hpi(x=strikesR) # also tekes several hours, but faster
saveRDS(H_pi_R, "h_pi_r.Rda")

