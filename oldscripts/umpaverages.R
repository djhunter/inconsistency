umpdata <- readRDS("aprilaugustumps.rds")
fixedumpnames <- str_replace(umpdata$ump, "Tripp Gibson III", "Tripp Gibson")
fixedumpnames <- str_replace(fixedumpnames, "Joseph West", "Joe West")
fixedumpnames <- str_replace(fixedumpnames, "Anthony Randazzo", "Tony Randazzo")
fixedumpnames <- str_replace(fixedumpnames, "Timothy Timmons", "Tim Timmons")
fixedumpnames <- str_replace(fixedumpnames, "Lazaro Diaz", "Laz Diaz")
fixedumpnames <- str_replace(fixedumpnames, "Daniel Bellino", "Dan Bellino")
fixedumpnames <- str_replace(fixedumpnames, "Douglas Eddings", "Doug Eddings")
fixedumpnames <- str_replace(fixedumpnames, "Edward Barrett", "Ted Barrett")
fixedumpnames <- str_replace(fixedumpnames, "Manuel Gonzalez", "Manny Gonzalez")
fixedumpnames <- str_replace(fixedumpnames, "Benjamin May", "Ben May")
fixedumpnames <- str_replace(fixedumpnames, "James Rackley", "David Rackley")

umpdata$ump <- fixedumpnames
uumps <- unique(umpdata$ump)
umpAverages <- data.frame(ump = uumps, aveI = numeric(length(uumps)))
for (i in 1:length(uumps)) {
  umpAverages[i,"aveI"] = mean(umpdata[umpdata$ump==uumps[i], "total" ], na.rm=TRUE)
}
print(umpAverages[order(umpAverages$aveI),])

