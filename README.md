# inconsistency
R Scripts for measuring inconsistency in MLB ball and strike calls. For more details, see my paper in the Journal of Quantitative Analysis in Sports: [New metrics for evaluating home plate umpire consistency and accuracy](https://doi.org/10.1515/jqas-2018-0061).

The file `pitches2017.Rda` contains pitch-tracking data for all pitches during the 2017 MLB season. This data was scraped from `http://gd2.mlb.com` using the `pitchRx` package. The scripts in the main directory of this repository will work if this file is read into the variable `pitches` and made into a `tibble`.

```R
library(dplyr)
library(tibble)
pitches <- as_tibble(readRDS("pitches2017.Rda"))
```

Scripts that require this data will have this `readRDS` line commented out, because it takes a while and it only needs to be executed once per session. Of course, other data can be loaded into the `pitches` variable, as long as it has the same column names as `pitches2017.Rda`.

Many of the scripts take a long time to run, so results for the 2017 season are saved as `.Rda` files. The following table lists the generating scripts.

| Data file | Generating script |
|-----------|-------------------|
| games17inc.Rda |  measure_games.R |
| umps17.Rda |  measure_umps.R |
| conzonepoly.Rda | consensus_zones.R |
| conczKDE.Rda | consensus_zones.R |

For other years, you can use the file `pitches2015-2018.Rda`, which you can make using the script `get2015-2018data.R`. For example, if you want to run everything on the year 2018, do this:

```R
library(dplyr)
library(tibble)
library(lubridate)
pitches <- as_tibble(readRDS("pitches2015-2018.Rda")) %>%
  filter(year(date) == 2018)
```
