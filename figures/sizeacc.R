library(ggplot2)
library(GGally)

umps17 <- readRDS("umps17.Rda")
regularUmps17 <- umps17[umps17$ngames >= 20,]

sizeacc <- ggpairs(regularUmps17[,c(11,10,16)], axisLabels = "none", 
                   columnLabels = c("Consensus accuracy", "Rule book accuracy", "Zone size")) +
            theme_bw()
ggsave("figures/sizeacc.pdf", plot = sizeacc, width = 6, height = 4.3, dpi = 300)

