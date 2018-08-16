library(ggplot2)
library(corrplot)
library(RColorBrewer)

umps17 <- readRDS("umps17.Rda")
regularUmps17 <- umps17[umps17$ngames >= 20,]
rumpdf <- regularUmps17[,c(5,6,8,9,11,10,14,16,17,18)]
names(rumpdf) <- c("I_R1", "I_R10", "I_CH", "I_ACH", "A_C", "A_R", "D_S", "S", "r_W", "r_K")
pdf("figures/allcor2.pdf", width=7, height=6)
corrplot.mixed(cor(rumpdf), upper = "number", lower="circle", tl.col = "black",
               upper.col = brewer.pal(n = 10, name = "PuOr"),
               lower.col = brewer.pal(n = 10, name = "PuOr")
               ) 
dev.off()

