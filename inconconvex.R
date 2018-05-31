library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda"))

library(alphahull)

#' Inconsistency index
#'
#' Computes inconsistency index for a single game using proportion of balls inside convex hull of strikes.
#'
#' @param gameID MLBAM game id.
#'
#' @return Returns inconsistency index 
#' @export
#' @import alphahull
#'
#' @examples
#' inconConvex("gid_2017_08_12_chnmlb_arimlb_1")
inconConvex <- function(gameID) {
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
  emptyhull <- ahull(c(-100, -101, -100), c(0,0,1), alpha=10000) # kludge: not really empty but no data will ever be in it
  if (nrow(Rstrikes)>2) RstrikeHull <- ahull(Rstrikes, alpha=10000) else RstrikeHull <- emptyhull
  if (nrow(Lstrikes)>2) LstrikeHull <- ahull(Lstrikes, alpha=10000) else LstrikeHull <- emptyhull
  badRballs <- sum(inahull(RstrikeHull, matrix(unlist(Rballs), ncol=2, byrow=FALSE)))
  badLballs <- sum(inahull(LstrikeHull, matrix(unlist(Lballs), ncol=2, byrow=FALSE)))
  incCH <- (badLballs+badRballs)/(nrow(Lballs)+nrow(Rballs))
  return(incCH)
}
