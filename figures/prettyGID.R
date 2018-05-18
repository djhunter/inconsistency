#' Beautify Game ID's 
#'
#' Returns a human-readable string given an MLB game id.
#'
#' @param gameID MLBAM game id.
#'
#' @return Returns a human-readable description of the game.
#' @export
#'
#' @examples
#' prettyGID("gid_2017_08_12_chnmlb_arimlb_1")
prettyGID <- function(gameID) {
  year <- substr(gameID, 5, 8)
  month <- as.numeric(substr(gameID, 10, 11))
  day <- as.numeric(substr(gameID, 13, 14))
  date <- paste(month, day, year, sep="/")
  home <- substr(gameID, 16,21)
  away <- substr(gameID, 23, 28)
}