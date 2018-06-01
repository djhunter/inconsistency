library(ggplot2)
library(gridExtra)

games17inc <- as_data_frame(readRDS("games17inc.Rda"))
games17inc <- subset(games17inc, npitch>=50) # throw out games with less than 50 pitches
games17inc <- subset(games17inc, npitch<=300) # throw out games with more than 300 pitches

rdf <- stack(games17inc, select=c(incR1, incR10))
names(rdf) <- c("inconsistency", "metric")
hdf <- stack(games17inc, select=c(incCH, incACH7))
names(hdf) <- c("inconsistency", "metric")
rectplots <- ggplot(rdf, aes(inconsistency, fill=metric, color=metric)) +
              geom_density(alpha=0.3) +
              xlim(0,0.4) +
              scale_fill_manual(labels=c(expression(italic(I[R1])), expression(italic(I[R10]))), values=c("orangered", "purple4")) +
              scale_color_manual(labels=c(expression(italic(I[R1])), expression(italic(I[R10]))), values=c("orangered", "purple4")) +
              theme_bw() +
              theme(legend.text = element_text(family="Bookman"))
chullplots <- ggplot(hdf, aes(inconsistency, fill=metric, color=metric)) +
              geom_density(alpha=0.3) +
              xlim(0,0.25) +
              scale_fill_manual(labels=c(expression(italic(I[CH])), expression(italic(I[ACH]))), values=c("orangered", "purple4")) +
              scale_color_manual(labels=c(expression(italic(I[CH])), expression(italic(I[ACH]))), values=c("orangered", "purple4")) +
              theme_bw() +
              theme(legend.text = element_text(family="Bookman"))

ggsave("figures/skewness.pdf", 
       plot=grid.arrange(rectplots, chullplots, ncol=2),
       width=8, height=3, dpi=300)