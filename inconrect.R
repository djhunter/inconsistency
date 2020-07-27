library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

#' Inconsistency index (rectangles)
#'
#' Computes inconsistency index using rectangle layers.
#'
#' @param gameID MLBAM game id.
#' @param layers Number of rectangle layers. Default is 3.
#'
#' @return Returns a list giving rectangle inconsistency index for game and a (layers X 4) matrix of rectangles.
#' @export
#'
#' @examples
#' inconRect("gid_2017_08_12_chnmlb_arimlb_1")
inconRect <- function(gameID, layers = 3) {
  pitchdata <- subset(pitches, gameday_link == gameID)
  # normalize up/down locations based on height of batter. Zone goes from 1.5 to 3.5.
  pitchdata$pz <- 2.0*(pitchdata$pz-pitchdata$sz_top)/(pitchdata$sz_top-pitchdata$sz_bot)+3.5
  Lpitchdata <- subset(pitchdata, stand=="L")
  Rpitchdata <- subset(pitchdata, stand=="R")
  Lballs <- Lpitchdata[Lpitchdata$des=="Ball" | Lpitchdata$des=="Ball In Dirt",c("px","pz")]
  Lstrikes <- Lpitchdata[Lpitchdata$des=="Called Strike",c("px","pz")]
  Rballs <- Rpitchdata[Rpitchdata$des=="Ball" | Rpitchdata$des=="Ball In Dirt",c("px","pz")]
  Rstrikes <- Rpitchdata[Rpitchdata$des=="Called Strike",c("px","pz")]
  Lballs <- Lballs[!is.na(Lballs[,1]),]
  Rballs <- Rballs[!is.na(Rballs[,1]),]
  Lstrikes <- Lstrikes[!is.na(Lstrikes[,1]),]
  Rstrikes <- Rstrikes[!is.na(Rstrikes[,1]),]
  llay <- min(layers, nrow(Lstrikes) %/% 2) # can't have more layers than (number of strikes)/2
  rlay <- min(layers, nrow(Rstrikes) %/% 2) #
  xlhh <- sort(Lstrikes$px)
  xrhh <- sort(Rstrikes$px)
  xmin_l <- xlhh[1:llay]
  xmin_r <- xrhh[1:rlay]
  xmax_l <- xlhh[length(xlhh):(length(xlhh)-llay+1)]
  xmax_r <- xrhh[length(xrhh):(length(xrhh)-rlay+1)]
  ylhh <- sort(Lstrikes$pz)
  yrhh <- sort(Rstrikes$pz)
  ymin_l <- ylhh[1:llay]
  ymin_r <- yrhh[1:rlay]
  ymax_l <- ylhh[length(ylhh):(length(ylhh)-llay+1)]
  ymax_r <- yrhh[length(yrhh):(length(yrhh)-rlay+1)]
  incR <- (sum(sapply(seq_along(rlay), function(i) 
    {Rballs$px < xmax_r[i] & Rballs$px > xmin_r[i] & Rballs$pz > ymin_r[i] & Rballs$pz < ymax_r[i]})) +
          sum(sapply(seq_along(llay), function(i) 
    {Lballs$px < xmax_l[i] & Lballs$px > xmin_l[i] & Lballs$pz > ymin_l[i] & Lballs$pz < ymax_l[i]}))) /
            (nrow(Lballs) + nrow(Rballs))
  M_lhh <- cbind(xmin_l, ymin_l, xmax_l, ymax_l)
  M_rhh <- cbind(xmin_r, ymin_r, xmax_r, ymax_r)
  return(list(incR = incR, M_lhh = M_lhh, M_rhh = M_rhh))
}
