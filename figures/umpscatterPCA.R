library(ggplot2)
library(ggfortify)

umps17 <- readRDS("umps17.Rda")
regularUmps17 <- umps17[umps17$ngames >= 20,]
rownames(regularUmps17) <- regularUmps17$umpname
pca1 <- prcomp(regularUmps17[,c(5,6,8:11,14,15,16)], scale=TRUE)

# Figures for the paper:
# xtable(cor(umpdf[,c(11,7,8,6)]))
# xtable(pca1$rotation)
# xtable(cor(umpdf[,c(6,11,9,12,10)]))

umpscat1 <- ggplot(regularUmps17, aes(y=(accCZ+accRB)/2, x=(aiR10+aiACH7), label=umpname))+xlim(0.12,0.28)+
   geom_point(alpha=0) +geom_text(aes(label=umpname),hjust=0.5, vjust=0.0, size=7)+
   labs(y="Accuracy", x = "Inconsistency")+
   theme(axis.text=element_text(size=18), axis.title=element_text(size=24,face="bold"))
ggsave("figures/umpscatter1.pdf", plot = umpscat1, width = 18, height = 12, dpi = 300)

umpscat2 <- autoplot(pca1, label=TRUE, label.size=7, label.hjust=0.5, label.vjust=0.0, shape=FALSE, xlim=c(-0.27,0.27),
         xlab = "Strike Zone Quality", ylab = "Pitcher Friendliness")+
         theme(axis.text=element_text(size=18),axis.title=element_text(size=24,face="bold"))
ggsave("figures/umpscatter2.pdf", plot = umpscat2, width = 18, height = 12, dpi = 300)
