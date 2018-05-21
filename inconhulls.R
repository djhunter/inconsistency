library(dplyr)
library(tibble)
#pitches <- as_data_frame(readRDS("pitches2017.Rda")) 
# set up samples for measuring overlap
deltaxy <- 0.01
taw <- 3 # test area width
tah <- 3 # test area height
pxsamp <- seq(-(taw/2), taw/2, by=deltaxy) # center test area at (0,2.5)
pysamp <- seq(2.5-taw,2.5+taw,by=deltaxy)
sampPts <- as.matrix(expand.grid(pxsamp, pysamp))

library(alphahull)

#' Inconsistency index
#'
#' Computes inconsistency index for a single game using area overlap of convex hull and alpha-convex hull.
#'
#' @param gameID MLBAM game id.
#' @param alpha Radius to use for alpha-convex hull when computing ball zone. Default is NULL (search for radius)
#' @param alpha_ratio Proportion of maximal convex hull radius to use for alpha. Default is 0.95. Ignored if alpha is not NULL.
#'
#' @return Returns a list giving inconsistency index for game and the alphas that were used on each side of the plate.
#' @export
#' @import alphahull
#'
#' @examples
#' inconAlphaConvex("gid_2017_08_12_chnmlb_arimlb_1")
inconAlphaConvex <- function(gameID, alpha=NULL, alpha_ratio=0.95) {
  pitchdata <- subset(pitches, gameday_link == gameID)
  optimize_alpha = is.null(alpha)
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
  if (nrow(Rballs)<=2)
    RballHull <- emptyhull
  else {
    if (optimize_alpha) {
      # now search for biggest alpha so that center of zone is not in alpha-hull
      alphaR <- 10 # too big
      alphaL <- 0.01 # too small
      middle <- c(0,2) # center of strike zone
      epsilon <- 0.01 # accuracy for search
      while(alphaR - alphaL > epsilon) {
        alpha <- (alphaR+alphaL)/2
        RballHull <- ahull(na.omit(Rballs), alpha=alpha)
        if(inahull(RballHull, middle))
          alphaR <- alpha
        else
          alphaL <- alpha
      }
      alpha <- alphaL * alpha_ratio
    }
    RballHull <- ahull(Rballs, alpha=alpha)
  }
  alpha_RH <- alpha
    
  if (nrow(Lballs)<=2) 
    LballHull <- emptyhull
  else {
    if (optimize_alpha) {
      alphaR <- 10 # too big
      alphaL <- 0.01 # too small
      middle <- c(0,2) # center of strike zone
      epsilon <- 0.01 # accuracy for search
      while(alphaR - alphaL > epsilon) {
        alpha <- (alphaR+alphaL)/2
        LballHull <- ahull(Lballs, alpha=alpha)
        if(inahull(LballHull, middle))
          alphaR <- alpha
        else
          alphaL <- alpha
      }
      alpha <- alphaL * alpha_ratio
    }
    LballHull <- ahull(Lballs, alpha=alpha)
  }
  alpha_LH <- alpha
  
  Lcalls <- nrow(Lstrikes)+nrow(Lballs)
  Rcalls <- nrow(Rstrikes)+nrow(Rballs)
  incACH <- (sum(inahull(LstrikeHull, sampPts) & inahull(LballHull, sampPts))*Lcalls +
             sum(inahull(RstrikeHull, sampPts) & inahull(RballHull, sampPts))*Rcalls) *
            (taw * tah) /
            ((Lcalls + Rcalls)*nrow(sampPts))
  return(list(incACH = incACH, alpha_LH = alpha_LH, alpha_RH = alpha_RH))
}
