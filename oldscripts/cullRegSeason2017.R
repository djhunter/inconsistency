library(dplyr)
library(tibble)

rs2017 <- readRDS("regSeason2017.Rda")
colnames(rs2017) <- make.unique(colnames(rs2017))
pitches2017 <- select(as_data_frame(rs2017), -des_es, -id, -tfs, -tfs_zulu, -event_num, -sv_id, 
                      -play_guid, -cc, -mt, -url, -inning_side, -inning, -next_, -num, -on_1b, 
                      -on_2b, -nasty, -num.1, -start_tfs, -start_tfs_zulu, -atbat_des, 
                      -atbat_des_es, -event_num.1, -event_es, -play_guid.1, -url.1, -inning_side.1, 
                      -inning.1, -next_.1, -score, -event2, -event2_es, -batter_name, -pitcher_name, 
                      -gameday_link.1, -date, -end_tfs_zulu, -event3, -event3_es)
