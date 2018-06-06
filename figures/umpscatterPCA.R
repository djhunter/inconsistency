library(ggplot2)
library(ggfortify)

umps17 <- readRDS("umps17.Rda")
regularUmps17 <- umps17[umps17$ngames >= 20,]
rownames(regularUmps17) <- regularUmps17$umpname
pca1 <- prcomp(regularUmps17[,c(5,6,8:11,16:18)], scale=TRUE)

# Figures for the paper:
# xtable(pca1$rotation)

umpscat1 <- ggplot(regularUmps17, aes(x=accCZ, y=(aiR10+aiACH7), label=umpname))+
   xlim(0.904,0.931)+
   geom_point(alpha=0) +geom_text(aes(label=umpname),hjust=0.5, vjust=0.0, size=7) + 
   labs(x="Consensus Zone Accuracy", y = expression("Inconsistency " (italic(I[R10] + I[ACH]))))+
   theme_minimal() +
   theme(axis.text=element_text(size=18), axis.title=element_text(size=24)) + coord_flip() 
ggsave("figures/umpscatter1.pdf", plot = umpscat1, width = 18, height = 26, dpi = 300)

umpscat2 <- autoplot(pca1, label=TRUE, label.size=7, label.hjust=0.4, label.vjust=0.0, shape=FALSE, xlim=c(-0.27,0.27),
         xlab = "Strike Zone Quality", ylab = "Pitcher Friendliness")+
         theme_minimal() +
         theme(axis.text=element_text(size=18),axis.title=element_text(size=24)) 
ggsave("figures/umpscatter2.pdf", plot = umpscat2, width = 18, height = 26, dpi = 300)

# with(regularUmps17, cor(accCZ, aiR10 + aiACH7)) # -0.58
# summary(pca1) # PC1 and PC2 account for 68% of the variation