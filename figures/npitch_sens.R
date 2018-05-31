library(ggplot2)
library(gridExtra)

games17inc <- as_data_frame(readRDS("games17inc.Rda"))
games17inc <- subset(games17inc, npitch>=50) # throw out games with less than 50 pitches
games17inc <- subset(games17inc, npitch<=300) # throw out games with more than 300 pitches

incR1plot <- ggplot(games17inc, aes(x=npitch, y=incR1)) + geom_point(size = 0.5) +
              geom_smooth(method="loess") +
              labs(x = NULL, y=expression(italic(I[R1]))) + theme_bw() +
              theme(axis.title.y = element_text(angle = 0, vjust=0.43, family="Bookman")) +
              ggtitle("All games")
incR10plot <- ggplot(games17inc, aes(x=npitch, y=incR10)) + geom_point(size = 0.5) +
              geom_smooth(method="loess") +
              labs(x = NULL, y=expression(italic(I[R10]))) +theme_bw() +
              theme(axis.title.y = element_text(angle = 0, vjust=0.43, family="Bookman")) +
              ggtitle(" ")
incCHplot <- ggplot(games17inc, aes(x=npitch, y=incCH)) + geom_point(size = 0.5) +
              geom_smooth(method="loess") +
              labs(x = NULL, y=expression(italic(I[CH]))) +theme_bw() +
              theme(axis.title.y = element_text(angle = 0, vjust=0.43, family="Bookman")) +
              ggtitle(" ")
incACH7plot <- ggplot(games17inc, aes(x=npitch, y=incACH7)) + geom_point(size = 0.5) +
              geom_smooth(method="loess") +
              labs(x = NULL, y=expression(italic(I[ACH]))) +theme_bw() +
              theme(axis.title.y = element_text(angle = 0, vjust=0.43, family="Bookman")) +
              ggtitle(" ")

games17inc <- subset(games17inc, incR10+incACH7>0.3) # Consider the most inconsistent games

iincR1plot <- ggplot(games17inc, aes(x=npitch, y=incR1)) + geom_point(size = 0.5) +
              geom_smooth(method="loess") +
              labs(x = NULL, y=expression(italic(I[R1]))) + theme_bw() +
              theme(axis.title.y = element_text(angle = 0, vjust=0.43, family="Bookman")) +
              ggtitle("High-inconsistency games")
iincR10plot <- ggplot(games17inc, aes(x=npitch, y=incR10)) + geom_point(size = 0.5) +
              geom_smooth(method="loess") +
              labs(x = NULL, y=expression(italic(I[R10]))) +theme_bw() +
              theme(axis.title.y = element_text(angle = 0, vjust=0.43, family="Bookman")) +
              ggtitle(" ")
iincCHplot <- ggplot(games17inc, aes(x=npitch, y=incCH)) + geom_point(size = 0.5) +
              geom_smooth(method="loess") +
              labs(x = NULL, y=expression(italic(I[CH]))) +theme_bw() +
              theme(axis.title.y = element_text(angle = 0, vjust=0.43, family="Bookman")) +
              ggtitle(" ")
iincACH7plot <- ggplot(games17inc, aes(x=npitch, y=incACH7)) + geom_point(size = 0.5) +
              geom_smooth(method="loess") +
              labs(x = NULL, y=expression(italic(I[ACH]))) +theme_bw() +
              theme(axis.title.y = element_text(angle = 0, vjust=0.43, family="Bookman")) +
              ggtitle(" ")


ggsave("figures/npitch_sens.pdf", 
       plot=grid.arrange(incR1plot, incR10plot, incCHplot, incACH7plot, 
                         iincR1plot, iincR10plot, iincCHplot, iincACH7plot, ncol=4),
       width=11.2, height=4.7, dpi=300)