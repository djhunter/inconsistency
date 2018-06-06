library(ggplot2)
library(GGally)

umps17 <- readRDS("umps17.Rda")
regularUmps17 <- umps17[umps17$ngames >= 20,]
rumpdf <- regularUmps17[,c(5,6,8,9,11,10,14,16,17,18)]
names(rumpdf) <- c("I_R1", "I_R10", "I_CH", "I_ACH", "A_C", "A_R", "D_S", "S", "r_BB", "r_K")
sizeacc <- ggcorr(rumpdf, 
                  label=TRUE, label_size = 3, label_alpha = 0.7)
ggsave("figures/allcor.pdf", plot = sizeacc, width = 6, height = 5, dpi = 300)

