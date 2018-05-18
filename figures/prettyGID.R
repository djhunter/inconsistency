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
  away <- substr(gameID, 16, 18)
  home <- substr(gameID, 23, 25)
  team <- c("Pirates", "Nationals", "Rangers", "Twins", "Blue Jays", "Diamondbacks", "Brewers", 
             "Marlins", "Athletics", "Indians", "Red Sox", "Padres", "Cardinals", "Cubs", "Mariners",
             "Royals", "Rays", "Rockies", "Phillies", "Mets", "Astros", "White Sox", "Orioles", 
             "Dodgers", "Reds", "Braves", "Angels", "Tigers", "Yankees", "Giants")
  names(team) <- c("pit", "was", "tex", "min", "tor", "ari", "mil", "mia", "oak", "cle",
                    "bos", "sdn", "sln", "chn", "sea", "kca", "tba", "col", "phi", "nyn", 
                    "hou", "cha", "bal", "lan", "cin", "atl", "ana", "det", "nya", "sfn")
  return(paste0(team[away], " at ", team[home], ", ", date))
}