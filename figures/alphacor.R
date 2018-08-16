library(ggplot2)
library(GGally)

compare_alpha <- readRDS("compare_alpha.Rda")
cadf <- compare_alpha[,c(6:11)]
names(cadf) <- c("0.4", "0.5", "0.6", "0.7", "0.8", "0.9")
alphcc <- ggcorr(cadf, label=TRUE, label_size = 3, label_alpha = 0.7)
ggsave("figures/alphacor.pdf", plot = alphcc, width = 6, height = 5, dpi = 300)


