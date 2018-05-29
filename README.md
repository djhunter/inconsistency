# inconsistency
R Scripts for measuring inconsistency in MLB ball and strike calls. 

The file `pitches2017.Rda` contains pitch-tracking data for all pitches during the 2017 MLB season. This data was scraped from `http://gd2.mlb.com`. The scripts in the main directory of this repository will work if this file is read into the variable `pitches` and made into a `tibble`. 

```R
library(dplyr)
library(tibble)
pitches <- as_data_frame(readRDS("pitches2017.Rda"))
```

Scripts that require this data will have this `readRDS` line commented out, because it takes a while and it only needs to be executed once per session. Of course, other data can be loaded into the `pitches` variable, as long as it has the same column names as `pitches2017.Rda`.

Many of the scripts take a long time to run, so results for the 2017 season are saved as `.Rda` files. The following table lists the generating scripts.

| Data file | Generating script |
|-----------|-------------------|
| games17inc.Rda |  measure_games.R |
| umps17.Rda |  measure_umps.R |
| conzonepoly.Rda | consensus_zones.R |
