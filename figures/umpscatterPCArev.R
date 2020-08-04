library(ggplot2)
library(ggfortify)

## revisions requested by copy editor
library(ggrepel)

umps17 <- readRDS("umps17.Rda")
regularUmps17 <- umps17[umps17$ngames >= 20,]
rownames(regularUmps17) <- regularUmps17$umpname
pca1 <- prcomp(regularUmps17[,c(5,6,8:11,16:18)], scale=TRUE)

# Figures for the paper:
# xtable(pca1$rotation)

umpscat1 <- ggplot(regularUmps17, aes(x=accCZ, y=(aiR10+aiACH7), label=umpname))+
   geom_text_repel(aes(label=umpname), size=8) +
   xlim(0.904,0.931)+
   # geom_point(alpha=0) + 
   geom_point(color = 'darkslateblue', size=3) + 
#   geom_text(aes(label=umpname),hjust=0.5, vjust=0.0, size=7) + 
   labs(x="Consensus Zone Accuracy", y = expression("Inconsistency " (italic(I[R10] + I[ACH]))))+
   theme_minimal() +
   theme(axis.text=element_text(size=18), axis.title=element_text(size=24)) + coord_flip() 
ggsave("figures/umpscatter1.pdf", plot = umpscat1, width = 18, height = 26, dpi = 300)
ggsave("slides/umpscattercor.png", plot = umpscat1, width = 1920/72, height = 1080/72, units = "in", dpi=72)

usdf <- data.frame(szq = pca1$x[,1], pf = pca1$x[,2], umpname = rownames(pca1$x))

umpscat2 <- ggplot(usdf, aes(x=szq, y=pf, label=umpname))+
  geom_text_repel(aes(label=umpname), size=8) +
  xlim(-5,5) +
  geom_point(color = 'darkslateblue', size=3) + 
  labs(x="Strike Zone Quality", y = "Pitcher Friendliness")+
         geom_hline(yintercept=0) +
         geom_vline(xintercept=0) +
  theme_minimal() +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=24)) 

# for paper:
ggsave("figures/umpscatter2.pdf", plot = umpscat2, width = 18, height = 26, dpi = 300)
# for slides:
ggsave("slides/umpscatterpca.png", plot = umpscat2, width = 1920/72, height = 1080/72, units = "in", dpi=72)

# with(regularUmps17, cor(accCZ, aiR10 + aiACH7)) # -0.58
# summary(pca1) # PC1 and PC2 account for 68% of the variation